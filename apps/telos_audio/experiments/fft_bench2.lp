// 7 Vars Total

// Minimize bandwidth of cut:
min: 10000000 - 10000000 TOS_readstream_uint16_10
    + 10000000 - 10000000 TOS_readstream_uint16_10
    + 10000000 TOS_readstream_uint16_10 - 10000000 Node_sensor_8 
    + 10000000 Node_sensor_8 - 10000000 Node_prep_5 
    + 6240 Node_prep_5 - 6240 Node_fft1_4 
    + 6216 Node_fft1_4 - 6216 Node_fft2_3 
    + 6192 Node_fft2_3 - 6192 Node_fft3_2 
    + 6168 Node_fft3_2 - 6168 main_1 
    + 24 main_1 ;

// Make them binary (should be able to do this with an bin x; assert):
0 <= TOS_readstream_uint16_10 <= 1;
0 <= Node_sensor_8 <= 1;
0 <= Node_prep_5 <= 1;
0 <= Node_fft1_4 <= 1;
0 <= Node_fft2_3 <= 1;
0 <= Node_fft3_2 <= 1;
0 <= main_1 <= 1;

// Require that we only cut each path once:
1 - TOS_readstream_uint16_10 >= 0;
1 - TOS_readstream_uint16_10 >= 0;
TOS_readstream_uint16_10 - Node_sensor_8 >= 0;
Node_sensor_8 - Node_prep_5 >= 0;
Node_prep_5 - Node_fft1_4 >= 0;
Node_fft1_4 - Node_fft2_3 >= 0;
Node_fft2_3 - Node_fft3_2 >= 0;
Node_fft3_2 - main_1 >= 0;

// Require that CPU sums to less than 100%:
MAGIC >=  0 TOS_readstream_uint16_10 
    +   0 Node_sensor_8 
    +   0 Node_prep_5 
    +   272 Node_fft1_4 
    +   369 Node_fft2_3 
    +   700 Node_fft3_2 
    +   0 main_1 ;

// Make all vars integer (should be binary)
int TOS_readstream_uint16_10, Node_sensor_8, Node_prep_5, Node_fft1_4, Node_fft2_3, Node_fft3_2, main_1;
