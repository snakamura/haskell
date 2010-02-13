const float PI = 3.1415926535;
uniform float u_counter;
uniform mat4 u_mvp;
attribute vec4 a_position;
varying vec2 v_texCoord;
void main() {
  vec4 pos = a_position;
  pos.z = sin((pos.x - mod(u_counter, 128.0)/20.0)*PI*4)*(pos.x + 1.0);
  gl_Position = u_mvp*pos;
  v_texCoord = vec2((pos.x + 1.0)/2, (1.0 - pos.y)/2);
}
