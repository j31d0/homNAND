#include "HomNAND_EFastCircuit.h"

JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_feval
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[16];
out[0] = (!(in[0] && in[16]));
out[1] = (in[5] ^ in[21]);
out[2] = (in[15] ^ in[31]);
out[3] = (in[10] ^ in[26]);
out[4] = (in[3] ^ in[19]);
out[5] = (in[1] ^ in[17]);
out[6] = (in[7] ^ in[23]);
out[7] = (in[12] ^ in[28]);
out[8] = (in[11] ^ in[27]);
out[9] = (in[8] ^ in[24]);
out[10] = (in[6] ^ in[22]);
out[11] = (in[2] ^ in[18]);
out[12] = (in[13] ^ in[29]);
out[13] = (in[14] ^ in[30]);
out[14] = (in[9] ^ in[25]);
out[15] = (in[4] ^ in[20]);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,16);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,16,(jboolean*)(&out));
return outp;
}
