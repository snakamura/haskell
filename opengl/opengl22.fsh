const vec3 c_light = normalize(vec3(2.0, 0.0, -1.0));
const vec4 c_materialDiffuse = vec4(0.1, 0.1, 0.1, 1.0);
const vec4 c_materialAmbient = vec4(0.9, 0.9, 0.9, 1.0);

uniform sampler2D u_texture;

varying vec2 v_texCoord;
varying vec3 v_normal;

void main() {
	float intensity = max(dot(c_light, normalize(v_normal)), 0.0);
	vec3 cf = intensity*c_materialDiffuse.rgb + c_materialAmbient.rgb;
	float af = c_materialDiffuse.a;

	vec4 color = texture2D(u_texture, v_texCoord);

	gl_FragColor = vec4(color.rgb*cf, color.a*af);
}
