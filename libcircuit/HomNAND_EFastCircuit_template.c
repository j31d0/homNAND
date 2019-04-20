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
  memcpy(&ina[N], &in[0], N * sizeof(jboolean));
  memcpy(&inb[0], &in[N], N * sizeof(jboolean));
  memcpy(&inb[N], &in[N], N * sizeof(jboolean));
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
static void fastmux16(jboolean in[], jboolean out[]) {
  int i = 0;
  jboolean tmp[N * 3];
  for(i = 0; i < 16; i ++) {
    memcpy(tmp, &in[i * N], N * sizeof(jboolean));
    memcpy(&tmp[N], &in[16 * N + i * N], N * sizeof(jboolean));
    memcpy(&tmp[2 * N], &in[32 * N], N * sizeof(jboolean));
    fastmux(tmp, &out[i * N]);
  }
}
static void fastmux4way16(jboolean in[], jboolean out[]) {
  jboolean tmp[32 * N + N];
  jboolean otmp[32 * N + N];
  memcpy(tmp , &in[0], 32 * N * sizeof(jboolean));
  memcpy(&tmp[32 * N], &in[64 * N], N * sizeof(jboolean));
  fastmux16(tmp, otmp);
  memcpy(tmp , &in[32 * N], 32 * N * sizeof(jboolean));
  memcpy(&tmp[32 * N], &in[64 * N], N * sizeof(jboolean));
  fastmux16(tmp, &otmp[16 * N]);
  memcpy(&otmp[32 * N], &in[64 * N + N], N * sizeof(jboolean));
  fastmux16(otmp, out);
}
static void fastmux8way16(jboolean in[], jboolean out[]) {
  jboolean tmp[64 * N + 2 * N];
  jboolean otmp[32 * N + N];
  memcpy(tmp , &in[0], 64 * N * sizeof(jboolean));
  memcpy(&tmp[64 * N], &in[128 * N], 2 * N * sizeof(jboolean));
  fastmux4way16(tmp, otmp);
  memcpy(tmp , &in[64 * N], 64 * N * sizeof(jboolean));
  memcpy(&tmp[64 * N], &in[128 * N], 2 * N * sizeof(jboolean));
  fastmux4way16(tmp, &otmp[16 * N]);
  memcpy(&otmp[32 * N], &in[128 * N + 2 * N], N * sizeof(jboolean));
  fastmux16(otmp, out);
}

static void fastmux64way16(jboolean in[], jboolean out[]) {
  jboolean tmp[128 * N + 3 * N];
  jboolean otmp[128 * N + 3 * N];
  int i = 0;
  for(i = 0; i < 8; i++) {
    memcpy(tmp , &in[i * 128 * N], 128 * N * sizeof(jboolean));
    memcpy(&tmp[128 * N], &in[1024 * N], 3 * N * sizeof(jboolean));
    fastmux8way16(tmp, &otmp[i * 16 * N]);
  }
  memcpy(&otmp[128 * N], &in[1024 * N + 3 * N], 3 * N * sizeof(jboolean));
  fastmux8way16(otmp, out);
}

static void fastmux512way16(jboolean in[], jboolean out[]) {
  jboolean tmp[1024 * N + 6 * N];
  jboolean otmp[128 * N + 3 * N];
  int i = 0;
  for(i = 0; i < 8; i++) {
    memcpy(tmp , &in[i * 1024 * N], 1024 * N * sizeof(jboolean));
    memcpy(&tmp[1024 * N], &in[8192 * N], 6 * N * sizeof(jboolean));
    fastmux64way16(tmp, &otmp[i * 16 * N]);
  }
  memcpy(&otmp[128 * N], &in[8192 * N + 6 * N], 3 * N * sizeof(jboolean));
  fastmux8way16(otmp, out);
}
static void fastmux4kway16(jboolean in[], jboolean out[]) {
  jboolean tmp[8192 * N + 9 * N];
  jboolean otmp[128 * N + 3 * N];
  int i = 0;
  for(i = 0; i < 8; i++) {
    memcpy(tmp , &in[i * 8192 * N], 8192 * N * sizeof(jboolean));
    memcpy(&tmp[8192 * N], &in[65536 * N], 9 * N * sizeof(jboolean));
    fastmux512way16(tmp, &otmp[i * 16 * N]);
  }
  memcpy(&otmp[128 * N], &in[65536 * N + 9 * N], 3 * N * sizeof(jboolean));
  fastmux8way16(otmp, out);
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
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastmux16
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N * 16];
fastmux16(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N * 16);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N * 16,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastmux4way16
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N * 16];
fastmux4way16(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N * 16);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N * 16,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastmux8way16
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N * 16];
fastmux8way16(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N * 16);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N * 16,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastmux64way16
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N * 16];
fastmux64way16(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N * 16);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N * 16,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastmux512way16
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N * 16];
fastmux512way16(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N * 16);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N * 16,(jboolean*)(&out));
return outp;
}
JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastmux4kway16
  (JNIEnv * env, jobject obj, jbooleanArray arr) {
jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);
jboolean out[N * 16];
fastmux4kway16(in, out);
jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,N * 16);
(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);
(*env)->SetBooleanArrayRegion(env,outp,0,N * 16,(jboolean*)(&out));
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
