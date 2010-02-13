uniform sampler2D u_texture;

varying vec2 v_texCoord;
varying vec4 v_diffuseColor;

void main() {
	gl_FragColor = texture2D(u_texture, v_texCoord) + v_diffuseColor;
}
