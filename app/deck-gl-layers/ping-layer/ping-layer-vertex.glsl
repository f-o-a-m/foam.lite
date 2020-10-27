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

#define SHADER_NAME ping-layer-vertex-shader

attribute vec3 positions;

attribute vec3 instancePositions;
attribute float instanceRadius;
attribute vec4 instanceColors;
attribute vec3 instancePickingColors;

uniform float opacity;
uniform float radiusScale;
uniform float radiusMinPixels;
uniform float radiusMaxPixels;
uniform float outline;
uniform float strokeWidth;
uniform float currentTime;

varying vec4 vColor;
varying vec2 unitPosition;
varying float innerUnitRadius;
varying float cyclePosition;

void main(void) {
  // Multiply out radius and clamp to limits
  cyclePosition = mod(currentTime + instancePositions.x * 50000. , 150.) / 100.;

  float outerRadiusPixels = clamp(project_scale(radiusScale * instanceRadius),
                                  radiusMinPixels, radiusMaxPixels);

  // outline is centered at the radius
  // outer radius needs to offset by half stroke width
  outerRadiusPixels += strokeWidth / 2.0;

  // position on the containing square in [-1, 1] space
  unitPosition = positions.xy;


  // 0 - solid circle, 1 - stroke with lineWidth=0
  innerUnitRadius = (1.0 - strokeWidth / outerRadiusPixels);

  // Find the center of the point and add the current vertex
  vec3 center = project_position(instancePositions);
  vec3 vertex = positions * outerRadiusPixels;
  gl_Position = project_to_clipspace(vec4(center + vertex, 1.0));

  // Apply opacity to instance color, or return instance picking color
  vColor = vec4(instanceColors.rgb, instanceColors.a * opacity) / 255.;

  /*
  // Set color to be rendered to picking fbo (also used to check for selection highlight).
  picking_setPickingColor(instancePickingColors);
  */
}
