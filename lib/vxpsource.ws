
include "timebase.ws";

c_isnull :: (Pointer "void *") -> Bool = 
  foreign("vxp_isnull", []);

fun vxp_c_interface(spill_mode) {
"
#include <devel/wavescope/wavescope_ensbox.h>
#include <pthread.h>

/*
 *  Queuing vxp data from second thread back into main event loop.
 */

struct queued_data {
  uint64_t sample_count; // the value according to sample counter..
  char *buf; // pointer to data
  int length; // length of data buffer
  int count; // number of samples in buffer
};

#define MAX_QUEUE_LEN (8*MILLION_I)

pthread_mutex_t arg_mutex;
pthread_cond_t arg_cond;
FILE *remote_stream = NULL;
FILE *remote_index = NULL;
char curr_fn[256] = {};
int file_index = 0;  // byte index into current file 
int last_time_check = 0;

int vxp_isnull(void *p) { return (p == NULL) ? 1 : 0; }

static
int audio_from_queue(msg_queue_opts_t *opts, buf_t *buf)
{  
  ws_state_t *ws = (ws_state_t *)(opts->private_data);
  if (buf) {
    if (buf->len == sizeof(struct queued_data)) {
      struct queued_data *qd = (struct queued_data *)buf->buf;
      g_msg_queue_dec_size(ws->mq, qd->length);
      
      if (qd->buf) {

#ifdef ENABLE_TIMESYNC
	int now = time(0);
	if (now - last_time_check > 5) {
	  last_time_check = now;

	  /* inject into timebase system */
	  double gps;
	  double cpu;
	  double samples = qd->sample_count;
	  if (samples_to_clock_value(samples, GPS, &gps) == 0) {
#ifdef TIMEBASEDEBUG
elog(LOG_WARNING, \"adding entry %d->%d %lf %lf\",
CLOCK_VXP, CLOCK_GPS, samples, gps);
#endif
  	    timebase_add_segment(CLOCK_VXP, samples, CLOCK_GPS, gps);
	  }
	  if (samples_to_clock_value(samples, CPU, &cpu) == 0) {
  	    timebase_add_segment(CLOCK_VXP, samples, CLOCK_CPU, cpu);
	  }
	
	  samples += qd->count;
	  if (samples_to_clock_value(samples, GPS, &gps) == 0) {
  	    timebase_add_segment(CLOCK_VXP, samples, CLOCK_GPS, gps);
	  }
	  if (samples_to_clock_value(samples, CPU, &cpu) == 0) {
  	    timebase_add_segment(CLOCK_VXP, samples, CLOCK_CPU, cpu);
	  }
	}
#endif

	/* upcall to wavescript */
	WRAP_WSENTRY(__vxpentry(qd->buf, qd->count, qd->sample_count));
	free(qd->buf);
      }
      else {
	elog(LOG_CRIT, \"got msg queue element, null pointer\");
      }
    }

    else {
      elog(LOG_CRIT, \"got msg queue element, wrong size: %d\", buf->len);
    }
    
    buf_free(buf);
  }

  else {
    elog(LOG_CRIT, \"got null msg queue element\");
  }

  return EVENT_RENEW;
}


void audio_push(void *data, char *buf, int count, uint64_t sample_count) 
{
  ws_state_t *ws = (ws_state_t *)data;

  if (g_msg_queue_get_size(ws->mq) < MAX_QUEUE_LEN) {
    buf_t *b = buf_new();
    
    /* copy and queue it */
    struct queued_data qd = {
      sample_count: sample_count,
      length: count * 8,
      count: count
    };
    qd.buf = malloc(qd.length);
    memmove(qd.buf, buf, qd.length);  
    bufcpy(b, &qd, sizeof(qd));
    g_msg_queue_inc_size(ws->mq, qd.length);
    
    g_msg_queue_push(ws->mq, b);
  }

  else {
    elog(LOG_WARNING, \"Dropping %d samples.. no space in queue\", count);
  }
}


/* this thread will run the vxp server code, and pass data back via a message queue
   to the main event loop. */
static 
void *vxp_thread(void *arg) {

  // we're the sub-thread
  g_init_thread_context(0);

  ws_state_t *ws = (ws_state_t *)arg;
  

  // start the vxpc_server in library form
  ensbox_main(&(ws->argc), ws->argv);
  pthread_cond_signal(&arg_cond);
  //  pthread_mutex_unlock(&arg_mutex);
  ensbox_start(ws);
  
  return NULL;
}


int __initvxp()
{
  ws_state_t *ws = get_global_ws();

  /* message queue from vxp thread */
  msg_queue_opts_t mq_opts = {
    cb: audio_from_queue,
    private_data: ws,
    name: \"audio queue\",
  };

  if (g_msg_queue(&mq_opts, &(ws->mq)) < 0) {
    elog(LOG_CRIT, \"failed to create message queue\");
    exit(1);
  }

  // init cond variables before...
  pthread_mutex_init(&arg_mutex, NULL);
  pthread_cond_init(&arg_cond, NULL);

  // in main thread - broadcast..

  // pthread waits..

  /* spawn thread to run vxp. */
  if (pthread_create(&(ws->vxp_thread), NULL, vxp_thread, ws) < 0) {
    elog(LOG_CRIT, \"couldn't launch thread: %m\");
  }
  pthread_mutex_lock(&arg_mutex);
  pthread_cond_wait(&arg_cond, &arg_mutex);

}

"};


fun nullsafe_ptrToArray(p, len)
{
  if (c_isnull(p)) then {
    Array:make(len, gint(0))
  }
  else {
    ptrToArray(p,len)
  }
};


//vxp_source :: () -> List (Stream (Sigseg Int16));
fun vxp_source_init(spill_mode) {
  ccode = inline_C(vxp_c_interface(spill_mode), "__initvxp");
  src = (foreign_source("__vxpentry", []) :: Stream (Pointer "int16_t*" * Int * Int64));
  interleaved = iterate (p,len,counter) in src {
    // note: len and counter are measured in 4 channel samples!

    // check for NULL pointer
    if (c_isnull(p)) then {
      // could also use nullsafe_ptrToArray... 
    }

    else {
      arr :: Array Int16 = ptrToArray(p,len*4);
      emit(toSigseg(arr, counter*gint(4), tb_vxp));
    }
  };
  merge(ccode, interleaved);
}

fun vxp_source_stream(interleaved, offset) {
  one_deinterleaveSS2(4, offset, interleaved)
}

