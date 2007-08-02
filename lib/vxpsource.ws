

fun vxp_c_interface() {
"
#include <devel/wavescope/wavescope_ensbox.h>

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

static
int audio_from_queue(msg_queue_opts_t *opts, buf_t *buf)
{  
  ws_state_t *ws = (ws_state_t *)(opts->private_data);
  if (buf) {
    if (buf->len == sizeof(struct queued_data)) {
      struct queued_data *qd = (struct queued_data *)buf->buf;
      g_msg_queue_dec_size(ws->mq, qd->length);
      
      if (qd->buf) {
	/* upcall to wavescript */
	__vxpentry(qd->buf, qd->count, qd->sample_count);
	//free(qd->buf);
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


static
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
  ensbox_main(ws->argc, ws->argv);
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

  /* spawn thread to run vxp. */
  if (pthread_create(&(ws->vxp_thread), NULL, vxp_thread, ws) < 0) {
    fprintf(stderr, \"couldn't launch thread: %m\n\");
  }
}

"};


fun vxp_source() {
  ccode = inline_C(vxp_c_interface(), "__initvxp");
  src = (foreign_source("__vxpentry", []) :: Stream (Pointer "int16_t*" * Int * Int64));
  interleaved = iterate (p,len,counter) in src {
    arr :: Array Int16 = ptrToArray(p,len);
    emit arr;
  };
  merge(ccode, interleaved)
}

