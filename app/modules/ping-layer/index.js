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

import {COORDINATE_SYSTEM, Layer} from 'deck.gl';
import {Model, Geometry} from '@luma.gl/core';
import {GL} from 'luma.gl/constants';

import vs from './ping-layer-vertex.glsl';
import fs from './ping-layer-fragment.glsl';


const DEFAULT_COLOR = [0, 0, 0, 255];

const defaultProps = {
    radiusScale: 1,
    radiusMinPixels: 0, //  min point radius in pixels
    radiusMaxPixels: Number.MAX_SAFE_INTEGER, // max point radius in pixels
    strokeWidth: 1,
    outline: false,
    currentTime: 0,

    getPosition: x => {console.log(x.position); return x.position},
    getRadius: x => x.radius || 1,
    getColor: x => x.color || DEFAULT_COLOR
};

export default class PingLayer extends Layer {
    getShaders(id) {
        const {shaderCache} = this.context;

        return {vs, fs, modules: [], shaderCache}; // 'project' module added by default.
    }

    initializeState() {
        const {gl} = this.context;
        this.setState({model: this._getModel(gl)});

        /* eslint-disable max-len */
        /* deprecated props check */
        // this._checkRemovedProp('radius', 'radiusScale');
        //this._checkRemovedProp('drawOutline', 'outline');

        this.state.attributeManager.addInstanced({
            instancePositions: {size: 3, accessor: 'getPosition', update: this.calculateInstancePositions},
            instanceRadius: {size: 1, accessor: 'getRadius', defaultValue: 1, update: this.calculateInstanceRadius},
            instanceColors: {size: 4, type: gl.UNSIGNED_BYTE, accessor: 'getColor', update: this.calculateInstanceColors}
        });
        /* eslint-enable max-len */
    }

    updateAttribute({props, oldProps, changeFlags}) {
    //    if (props.fp64 !== oldProps.fp64) {
    //        const {attributeManager} = this.state;
    //        attributeManager.invalidateAll();

    //        if (props.fp64 && props.coordinateSystem === COORDINATE_SYSTEM.LNGLAT) {
    //            attributeManager.addInstanced({
    //                instancePositions64xyLow: {
    //                    size: 2,
    //                    accessor: 'getPosition',
    //                    update: this.calculateInstancePositions64xyLow
    //                }
    //            });
    //        } else {
    //            attributeManager.remove([
    //                'instancePositions64xyLow'
    //            ]);
    //        }
    //
    //   }
    }

    updateState({props, oldProps, changeFlags}) {
        super.updateState({props, oldProps, changeFlags});
    //    if (props.fp64 !== oldProps.fp64) {
    //        const {gl} = this.context;
    //        this.setState({model: this._getModel(gl)});
    //    }
        this.updateAttribute({props, oldProps, changeFlags});
    }

    draw({uniforms}) {
        const {radiusScale, radiusMinPixels, radiusMaxPixels, outline, strokeWidth, currentTime} = this.props;
        this.state.model.render(Object.assign({}, uniforms, {
            outline: outline ? 1 : 0,
            strokeWidth,
            radiusScale,
            radiusMinPixels,
            radiusMaxPixels,
            currentTime
        }));
    }

    _getModel(gl) {
        // a square that minimally cover the unit circle
        const positions = [-1, -1, 0, -1, 1, 0, 1, 1, 0, 1, -1, 0];

        return new Model(gl, Object.assign(this.getShaders(), {
            id: this.props.id,
            geometry: new Geometry({
                drawMode: gl.TRIANGLE_FAN,
                attributes: {
                    positions: new Float32Array(positions)
                }
            }),
            isInstanced: true,
            shaderCache: this.context.shaderCache
        }));
    }

    calculateInstancePositions(attribute) {
        const {data, getPosition} = this.props;
        const {value} = attribute;
        let i = 0;
        for (const point of data) {
            const position = getPosition(point);
            value[i++] = position[0];
            value[i++] = position[1];
            value[i++] = position[2] || 0;
        }
    }

    calculateInstanceRadius(attribute) {
        const {data, getRadius} = this.props;
        const {value} = attribute;
        let i = 0;
        for (const point of data) {
            const radius = getRadius(point);
            value[i++] = isNaN(radius) ? 1 : radius;
        }
    }

    calculateInstanceColors(attribute) {
        const {data, getColor} = this.props;
        const {value} = attribute;
        let i = 0;
        for (const point of data) {
            const color = getColor(point) || DEFAULT_COLOR;
            value[i++] = color[0];
            value[i++] = color[1];
            value[i++] = color[2];
            value[i++] = isNaN(color[3]) ? 255 : color[3];
        }
    }
}

PingLayer.layerName = 'PingLayer';
PingLayer.defaultProps = defaultProps;
