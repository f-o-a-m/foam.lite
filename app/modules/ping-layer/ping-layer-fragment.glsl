// Copyright (c) 2015 - 2017 Uber Technologies, Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#define SHADER_NAME ping-layer-fragment-shader

#ifdef GL_ES
precision highp float;
#endif

varying vec4 vColor;
varying vec2 unitPosition;
varying float innerUnitRadius;
varying float cyclePosition;

void main(void) {
  float distToCenter = length(unitPosition);

  if (distToCenter > 1.0 || distToCenter > cyclePosition && distToCenter < innerUnitRadius) {
    discard;
  }

  if (distToCenter > innerUnitRadius) {
    gl_FragColor = vec4(vColor.rgb, (1. - smoothstep(1., 0., smoothstep(0.,1.,(cyclePosition - distToCenter) * 2.))) * vColor.a);
  //gl_FragColor = vColor;  // vec4(cyclePosition, 0.0, 0.0, 155.0);
  } else {
    gl_FragColor = vec4(vColor.rgb, (1. - smoothstep(0., 1., smoothstep(0.,1.,(cyclePosition - distToCenter) * 2.))) * vColor.a);
  }

  /*
  // use highlight color if this fragment belongs to the selected object.
  gl_FragColor = picking_filterHighlightColor(gl_FragColor);

  // use picking color if rendering to picking FBO.
  gl_FragColor = picking_filterPickingColor(gl_FragColor);
  */
}
