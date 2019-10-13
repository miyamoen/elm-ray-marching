module Shaders exposing
    ( cyclicWaveShader
    , juliaShader
    , mandelbrotShader
    , noiseShader
    , orbShader
    , ringShader
    , testShader
    )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import WebGL exposing (Shader)


type alias Uniforms =
    { time : Float
    , mouse : Vec2
    , resolution : Vec2
    }


testShader : Shader {} Uniforms {}
testShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        void main (void) {
            vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);
            vec2 color = (vec2(1.0) + p.xy) * 0.5;
            gl_FragColor = vec4(color, 0.0, 1.0);
        }
    |]


cyclicWaveShader : Shader {} Uniforms {}
cyclicWaveShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        void main (void) {
            vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);
            float t = sin(length(mouse - p) * 10.0 + time * 0.4);
            gl_FragColor = vec4(vec3(t), 1.0);
        }
    |]


orbShader : Shader {} Uniforms {}
orbShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        void main (void) {
            vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

            float t = 0.1 / length(mouse - p);
            gl_FragColor = vec4(vec3(t), 1.0);
        }
    |]


ringShader : Shader {} Uniforms {}
ringShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        void main (void) {
            vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

            float t = 0.02 / abs(abs(sin(time)) - length(p));
            gl_FragColor = vec4(vec3(t), 1.0);
        }
    |]


mandelbrotShader : Shader {} Uniforms {}
mandelbrotShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        // HSV カラー生成関数
        vec3 hsv(float h, float s, float v){
            vec4 t = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
            vec3 p = abs(fract(vec3(h) + t.xyz) * 6.0 - vec3(t.w));
            return v * mix(vec3(t.x), clamp(p - vec3(t.x), 0.0, 1.0), s);
        }

        void main (void) {
            vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

            // マンデルブロ集合
            int j = 0;                     // カウンタ
            vec2  x = p + vec2(-0.5, 0.0); // 原点を少しずらす
            float y = 1.5 -  mouse.x; // マウス座標を使って拡大度を変更
            vec2  z = vec2(0.0, 0.0);      // 漸化式 Z の初期値

            // 漸化式の繰り返し処理(今回は 360 回ループ)
            for(int i = 0; i < 360; i++){
                j++;
                if(length(z) > 2.0){break;}
                z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + x * y;
            }

            // 時間の経過で色を HSV 出力する
            float h = mod(time * 20.0, 360.0) / 360.0;
            vec3 rgb = hsv(h, 1.0, 1.0);

            // 漸化式で繰り返した回数をもとに輝度を決める
            float t = float(j) / 360.0;

            // 最終的な色の出力
            gl_FragColor = vec4(rgb * t, 1.0);
        }
    |]


juliaShader : Shader {} Uniforms {}
juliaShader =
    [glsl|
        precision mediump float;
        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        vec3 hsv(float h, float s, float v){
            vec4 t = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
            vec3 p = abs(fract(vec3(h) + t.xyz) * 6.0 - vec3(t.w));
            return v * mix(vec3(t.x), clamp(p - vec3(t.x), 0.0, 1.0), s);
        }

        void main(void){
            vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

            int j = 0;
            vec2 x = vec2(-0.345, 0.654);
            vec2 y = vec2(time * 0.005, 0.0);
            vec2 z = p;
            for(int i = 0; i < 360; i++){
                j++;
                if(length(z) > 2.0){break;}
                z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + x + y;
            }

            float h = abs(mod(time * 15.0 - float(j), 360.0) / 360.0);;
            vec3 rgb = hsv(h, 1.0, 1.0);
            gl_FragColor = vec4(rgb, 1.0);

        }
    |]


noiseShader : Shader {} Uniforms {}
noiseShader =
    [glsl|
        precision mediump float;
        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        const int   oct  = 8;
        const float per  = 0.5;
        const float PI   = 3.1415926;
        const float cCorners = 1.0 / 16.0;
        const float cSides   = 1.0 / 8.0;
        const float cCenter  = 1.0 / 4.0;

        // 補間関数
        float interpolate(float a, float b, float x){
            float f = (1.0 - cos(x * PI)) * 0.5;
            return a * (1.0 - f) + b * f;
        }

        // 乱数生成
        float rnd(vec2 p){
            return fract(sin(dot(p ,vec2(12.9898,78.233))) * 43758.5453);
        }

        // 補間乱数
        float irnd(vec2 p){
            vec2 i = floor(p);
            vec2 f = fract(p);
            vec4 v = vec4(rnd(vec2(i.x,       i.y      )),
                          rnd(vec2(i.x + 1.0, i.y      )),
                          rnd(vec2(i.x,       i.y + 1.0)),
                          rnd(vec2(i.x + 1.0, i.y + 1.0)));
            return interpolate(interpolate(v.x, v.y, f.x), interpolate(v.z, v.w, f.x), f.y);
        }

        // ノイズ生成
        float noise(vec2 p){
            float t = 0.0;
            for(int i = 0; i < oct; i++){
                float freq = pow(2.0, float(i));
                float amp  = pow(per, float(oct - i));
                t += irnd(vec2(p.x / freq, p.y / freq)) * amp;
            }
            return t;
        }

        // シームレスノイズ生成
        float snoise(vec2 p, vec2 q, vec2 r){
            return noise(vec2(p.x,       p.y      )) *        q.x  *        q.y  +
                   noise(vec2(p.x,       p.y + r.y)) *        q.x  * (1.0 - q.y) +
                   noise(vec2(p.x + r.x, p.y      )) * (1.0 - q.x) *        q.y  +
                   noise(vec2(p.x + r.x, p.y + r.y)) * (1.0 - q.x) * (1.0 - q.y);
        }

        void main(void){
            // noise
            // vec2 t = gl_FragCoord.xy + vec2(time * 10.0);
            // float n = noise(t);

            // seamless noise
            const float map = 256.0;
            vec2 t = mod(gl_FragCoord.xy + vec2(time * 10.0), map);
            float n = snoise(t, t / map, vec2(map));

            gl_FragColor = vec4(vec3(n), 1.0);
        }

    |]
