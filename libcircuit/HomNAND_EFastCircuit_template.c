#include "HomNAND_EFastCircuit.h"
#include <string.h>
#define N $N
#define N2 ($N * 2)

static void fastnand(jboolean in[], jboolean out[]) {
$Circuit
}

static void fastand(jboolean in[], jboolean out[]) {
  jboolean mid[N2];
  fastnand(in, mid);
  memcpy(&mid[N], mid, N * sizeof(jboolean));
  fastnand(mid, out);
}

static void fastor(jboolean in[], jboolean out[]) {
  jboolean ina[N2];
  jboolean inb[N2];
  jboolean mid[N2];
  memcpy(&ina[0], &in[0], N * sizeof(jboolean));
  memcpy(&ina[16], &in[0], N * sizeof(jboolean));
  memcpy(&inb[0], &in[16], N * sizeof(jboolean));
  memcpy(&inb[16], &in[16], N * sizeof(jboolean));
  fastnand(ina, &mid[0]);
  fastnand(inb, &mid[N]);
  fastnand(mid, out);
}


static void fastxor(jboolean in[], jboolean out[]) {
  jboolean ab1[N2];
  jboolean ab2[N2];
  fastnand(in, ab1);
  memcpy(&ab1[N], &in[0], N * sizeof(jboolean));
  fastnand(ab1, &ab2[0]);
  memcpy(&ab1[N], &in[N], N * sizeof(jboolean));
  fastnand(ab1, &ab2[N]);
  fastnand(ab2, out);
}

static void fastnot(jboolean in[], jboolean out[]) {
  jboolean a1[N2];
  memcpy(&a1[0], &in[0], N * sizeof(jboolean));
  memcpy(&a1[N], &in[0], N * sizeof(jboolean));
  fastnand(a1, out);
}

static void fastmux(jboolean in[], jboolean out[]) {
  jboolean *a = &in[0];
  jboolean *b = &in[N];
  jboolean *s = &in[N2];
  jboolean nots[N];
  fastnot(s, nots);
  jboolean tmp[N2];
  jboolean asnotbs[N2];
  memcpy(tmp, a, N * sizeof(jboolean));
  memcpy(&tmp[N], nots, N * sizeof(jboolean));
  fastnand(tmp, asnotbs);
  memcpy(tmp, b, N * sizeof(jboolean));
  memcpy(&tmp[N], s, N * sizeof(jboolean));
  fastnand(tmp, &asnotbs[N]);
  fastnand(asnotbs, out);
}
static void fastbit(jboolean in[], jboolean out[]) {
  //(ea.el mux (s, in, load), s)
  fastmux(in, out);
  memcpy(in, &out[N], N * sizeof(jboolean));
}
static void fastreg(jboolean in[], jboolean out[]) {
  //(ea.el mux (s, in, load), s)
  int i = 0;
  jboolean tmp[N * 3];
  for(i = 0; i < 16; i ++) {
    memcpy(tmp, &in[i * N], N * sizeof(jboolean));
    memcpy(&tmp[N], &in[16 * N + i * N], N * sizeof(jboolean));
    memcpy(&tmp[2 * N], &in[32 * N], N * sizeof(jboolean));
    fastmux(tmp, &out[i * N]);
  }
  memcpy(in, &out[N * 16], N * 16 * sizeof(jboolean));
}

JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastnand
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N];
fastnand(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastand
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N];
fastand(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastor
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N);
jboolean out[N];
fastor(in, out);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastxor
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N);
jboolean out[N];
fastxor(in, out);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastnot
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N];
fastnot(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastmux
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N];
fastmux(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastbit
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N2];
fastbit(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N2);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N2,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastreg
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N * 32];
fastreg(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N * 32);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N * 32,(jboolean*)(&out));
return outp;
}
