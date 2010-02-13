const float c_pi = 3.1415926535;
const float samplingDiff = 0.1;

uniform float u_counter;
uniform mat4 u_mvp;

attribute vec4 a_position;

varying vec2 v_texCoord;
varying vec3 v_normal;

float calcZ(float x) {
	return sin((x - mod(u_counter, 128.0)/20.0)*c_pi*4.0)*(x + 1.0);
}

void main() {
	vec4 pos = a_position;
	pos.z = calcZ(pos.x);
	gl_Position = u_mvp*pos;
	v_texCoord = vec2((pos.x + 1.0)/2.0, (1.0 - pos.y)/2.0);
	
	vec3 posPrev = vec3(pos.x - samplingDiff, pos.y, calcZ(pos.x - samplingDiff));
	vec3 posNext = vec3(pos.x + samplingDiff, pos.y, calcZ(pos.x + samplingDiff));
	vec3 diff = posNext - posPrev;
	v_normal = normalize(vec3(diff.z, 0, -diff.x));
}
