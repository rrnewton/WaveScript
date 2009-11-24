
c_vxp_get_tb :: () -> Timebase = 
  foreign("vxp_get_tb", []);

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
  double gps;
  double cpu;
};

#define MAX_QUEUE_LEN (8*MILLION_I)

int __vxp_tb = 0;
pthread_mutex_t arg_mutex;
pthread_cond_t arg_cond;
int spill_mode = 0; // "++spill_mode++";

int vxp_get_tb() { return __vxp_tb; }
int vxp_isnull(void *p) { return (p == NULL) ? 1 : 0; }

static
int audio_from_queue(msg_queue_opts_t *opts, buf_t *buf)
{  
  ws_state_t *ws = (ws_state_t *)(opts->private_data);
  if (buf) {
    if (buf->len == sizeof(struct queued_data)) {
      struct queued_data *qd = (struct queued_data *)buf->buf;
      
      if (qd->buf) {
	
	/* inject into timebase system */
	double gps;
	double cpu;
	double samples = qd->sample_count;
  	timebase_add_segment(__vxp_tb, samples, gps_timebase(), qd->gps);
  	timebase_add_segment(__vxp_tb, samples, cpu_timebase(), qd->cpu);

	samples += qd->count;
	if (samples_to_clock_value(samples, GPS, &gps) == 0) {
  	  timebase_add_segment(__vxp_tb, samples, gps_timebase(), gps);
	}
	if (samples_to_clock_value(samples, CPU, &cpu) == 0) {
  	  timebase_add_segment(__vxp_tb, samples, cpu_timebase(), cpu);
	}

	/* upcall to wavescript */
	__vxpentry(qd->buf, qd->count, qd->sample_count);

	/* pass to save thread */
	if (spill_mode) {
	  g_msg_queue_push(ws->mq2, buf);
	  buf = NULL;
        }
      }
      else {
	elog(LOG_CRIT, \"got msg queue element, null pointer\");
      }
    }

    else {
      elog(LOG_CRIT, \"got msg queue element, wrong size: %d\", buf->len);
    }
    
    if (buf) buf_free(buf);
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

    /* compute times */
    samples_to_clock_value(sample_count, GPS, &(qd.gps));
    samples_to_clock_value(sample_count, CPU, &(qd.cpu));

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

/* this thread will run the spill client, and free data */
static 
void *spill_thread(void *arg) {
  ws_state_t *ws = (ws_state_t *)arg;
  
  int remote_stream = -1;
  int remote_index = -1;
  char curr_fn[256] = {};
  int file_index = 0;  // byte index into current file 
  int last_index = 0;  // byte index into current file 
  
  while (1) {
    buf_t *buf = g_msg_queue_wait(ws->mq2, -1);
    if (buf) {
      if (buf->len == sizeof(struct queued_data)) {
        struct queued_data *qd = (struct queued_data *)buf->buf;
        g_msg_queue_dec_size(ws->mq, qd->length);
      
        if (qd->buf && qd->length > 0) {
	
          struct ws_audio_chunk_hdr hdr = {
	    payload_length_bytes: qd->length,
            node_id: my_node_id,
            sample_rate: 48000,  // should be from global??
            channels: 4,
            bytes_per_channel: 2,
            sample_start: qd->sample_count
          };	

	  memmove(hdr.magic, WS_AUDIO_MAGIC, sizeof(hdr.magic));
	  double_to_tv(&(hdr.cpu_start), qd->cpu);
	  double_to_tv(&(hdr.gps_start), qd->gps);

	  /* $$$ skipping end conversions */

	  elog(LOG_DEBUG(0), \"starting write to socket\");

	  if (remote_index < 0) {
	    char fn[256];
            sprintf(fn, \"/remote/vxp_%d_index\", my_node_id);
	    remote_index = remote_open(fn, \"append\");
	    if (remote_index < 0) {
	      elog(LOG_WARNING, \"Failed to open remote connection: %m\");
            }
	    else
  	      elog(LOG_NOTICE, \"Opened remote index connection\");
	  }

	  if (remote_stream < 0) {
	    if (qd->gps != 0)
	      sprintf(curr_fn, \"/remote/vxp_%d_%.6llf.wsaudio\",
	              my_node_id, qd->gps);
	    else
	      sprintf(curr_fn, \"/remote/vxp_%d_%.6llf_NOGPS.wsaudio\",
	              my_node_id, qd->cpu);
	    remote_stream = remote_open(curr_fn, \"\");
	    if (remote_stream < 0) {
	      elog(LOG_WARNING, \"Failed to open remote connection: %m\");
            }
	    else 
  	      elog(LOG_NOTICE, \"Opened remote stream connection\");
	    file_index = 0;
	    last_index = 0;
	  }

	  if (remote_index >= 0) {
	    if (file_index == 0 || (file_index - last_index) > 100000) {
	      char str[256];
	      sprintf(str, \"%lld %.6lf %d %s%c\",
		      hdr.sample_start, qd->gps, 
		      file_index, curr_fn, 10);
	      int status = write_to_fd(remote_index, str, strlen(str));
	      if (status < 0) {
	        elog(LOG_WARNING, \"Remote connection failed (I): %m\");	      
	        close(remote_index);
	        remote_index = -1;
	      }
	      last_index = file_index;
            }
	  }

	  if (remote_stream >= 0) {	  
	    file_index += sizeof(hdr);
	    int status = write_to_fd(remote_stream, &hdr, sizeof(hdr));
	    if (status < 0) {
	      elog(LOG_WARNING, \"Remote connection failed (H): %m\");
	      close(remote_stream);
	      remote_stream = -1;
	    } 
	  }

	  if (remote_stream >= 0) {	  
	    file_index += qd->length;
	    int status = write_to_fd(remote_stream, qd->buf, qd->length);
	    if (status < 0) {
	      elog(LOG_WARNING, \"Remote connection failed: %m\");	      
	      close(remote_stream);
	      remote_stream = -1;
	    } 
	  }

	  elog(LOG_DEBUG(0), \"wrote %d bytes total\", file_index);
	}
	if (qd->buf) free(qd->buf);
      }
      buf_free(buf);
    }
  }

  return NULL;
}


int __initvxp()
{
  ws_state_t *ws = get_global_ws();

  __vxp_tb = timebase_new(my_node_id, CLOCK_VXP, 0.1);

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

  if (spill_mode) {
    /* message queue from main thread to spill thread */
    msg_queue_opts_t mq_opts = {
      synchronous: 1,
      private_data: ws,
      name: \"spill queue\",
    };

    if (g_msg_queue(&mq_opts, &(ws->mq2)) < 0) {
      elog(LOG_CRIT, \"failed to create message queue\");
      exit(1);
    }
  }

  // init cond variables before...
  pthread_mutex_init(&arg_mutex, NULL);
  pthread_cond_init(&arg_cond, NULL);

  // in main thread - broadcast..

  // pthread waits..

  if (spill_mode) {
    if (pthread_create(&(ws->spill_thread), NULL, spill_thread, ws) < 0) {
      elog(LOG_CRIT, \"couldn't launch thread: %m\");
    }
  }

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
      emit(toSigseg(arr, counter*gint(4), c_vxp_get_tb()));
    }
  };
  merge(ccode, interleaved);
}

fun vxp_source_stream(interleaved, offset) {
  one_deinterleaveSS2(4, offset, interleaved)
}

