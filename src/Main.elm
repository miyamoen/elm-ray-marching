module Main exposing (..)

{- -}

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Html exposing (Html)
import Html.Attributes as HAttrs exposing (style)
import Html.Events
import Json.Decode as JD
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


size : number
size =
    512


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type alias Model =
    { currentTime : Float
    , renderState : RenderState
    , mouse : Vec2
    }


type RenderState
    = RenderRunning
    | RenderPause


type Msg
    = Elapse Float
    | RunRender
    | PauseRender
    | MoveMouse Vec2


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentTime = 0
      , renderState = RenderPause
      , mouse = vec2 0 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Elapse elapsed ->
            ( { model | currentTime = model.currentTime + 0.001 * elapsed }, Cmd.none )

        RunRender ->
            ( { model | renderState = RenderRunning }, Cmd.none )

        PauseRender ->
            ( { model | renderState = RenderPause }, Cmd.none )

        MoveMouse mouse ->
            ( { model | mouse = mouse }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.renderState of
        RenderRunning ->
            onAnimationFrameDelta Elapse

        RenderPause ->
            Sub.none


view : Model -> Html Msg
view model =
    layout [ Background.color <| rgb255 255 255 210, padding 16 ] <|
        column [ width fill, spacing 32 ]
            [ el [ centerX ] <|
                html <|
                    WebGL.toHtmlWith
                        [ WebGL.clearColor 0 0 0 1 ]
                        [ HAttrs.width size
                        , HAttrs.height size
                        , HAttrs.style "display" "block"
                        , HAttrs.style "width" "300px"
                        , Html.Events.on "mousemove" <| mouseDecoder MoveMouse
                        ]
                        [ WebGL.entity
                            vertexShader
                            fragmentShader
                            mesh
                            { time = model.currentTime
                            , mouse = model.mouse
                            , resolution = vec2 size size
                            }
                        ]
            , column [ spacing 16 ]
                [ case model.renderState of
                    RenderRunning ->
                        row [ spacing 16, onClick PauseRender, pointer ]
                            [ text "⏯️"
                            , text "running"
                            ]

                    RenderPause ->
                        row [ spacing 16, onClick RunRender, pointer ]
                            [ text "⏯️"
                            , text "pause"
                            ]
                , row [ spacing 16 ]
                    [ text "current time"
                    , text ":"
                    , text <| String.fromFloat model.currentTime
                    ]
                , row [ spacing 16 ]
                    [ text "mouse"
                    , text ":"
                    , row [ spacing 8 ]
                        [ text <| String.fromFloat <| Vec2.getX model.mouse
                        , text ","
                        , text <| String.fromFloat <| Vec2.getY model.mouse
                        ]
                    ]
                ]
            ]


mouseDecoder : (Vec2 -> msg) -> JD.Decoder msg
mouseDecoder tagger =
    JD.map2 vec2
        (JD.field "offsetX" JD.float
            |> JD.map (\x -> (x / size) * 2.0 - 1.0)
        )
        (JD.field "offsetY" JD.float
            |> JD.map (\y -> -(y / size) * 2.0 + 1.0)
        )
        |> JD.map tagger



-- Mesh


type alias Vertex =
    { position : Vec3 }


mesh : Mesh Vertex
mesh =
    WebGL.indexedTriangles
        [ Vertex (vec3 -1 1 0)
        , Vertex (vec3 1 1 0)
        , Vertex (vec3 -1 -1 0)
        , Vertex (vec3 1 -1 0)
        ]
        [ ( 0, 2, 1 )
        , ( 1, 2, 3 )
        ]



-- Shaders


type alias Uniforms =
    { time : Float
    , mouse : Vec2
    , resolution : Vec2
    }


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;

        void main (void) {
            gl_Position = vec4(position, 1.0);
        }
    |]


testShader : Shader {} Uniforms {}
testShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        void main(void) {
            // fragment position
            vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

            // camera
            vec3 cPos = vec3(0.0,  0.0,  3.0); // カメラの位置
            vec3 cDir = vec3(0.0,  0.0, -1.0); // カメラの向き(視線)
            vec3 cUp  = vec3(0.0,  1.0,  0.0); // カメラの上方向
            vec3 cSide = cross(cDir, cUp);     // 外積を使って横方向を算出
            float targetDepth = 0.2;           // フォーカスする深度

            // ray
            vec3 ray = normalize(cSide * p.x + cUp * p.y + cDir * targetDepth);

            // color
            gl_FragColor = vec4(ray.xy, -ray.z, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float time;
        uniform vec2  mouse;
        uniform vec2  resolution;

        const vec3 lightDir = vec3(-0.577, 0.577, 0.577);
        const float PI = 3.14159265;
        const float fov = 120.0 * 0.5 * PI / 180.0;

        // rotate
        vec3 rotate(vec3 p, float angle, vec3 axis){
            vec3 a = normalize(axis);
            float s = sin(angle);
            float c = cos(angle);
            float r = 1.0 - c;
            mat3 m = mat3(
                a.x * a.x * r + c,
                a.y * a.x * r + a.z * s,
                a.z * a.x * r - a.y * s,
                a.x * a.y * r - a.z * s,
                a.y * a.y * r + c,
                a.z * a.y * r + a.x * s,
                a.x * a.z * r + a.y * s,
                a.y * a.z * r - a.x * s,
                a.z * a.z * r + c
            );
            return m * p;
        }

        // smoothing min
        float smoothMin(float d1, float d2, float k){
            float h = exp(-k * d1) + exp(-k * d2);
            return -log(h) / k;
        }

        // floor distance function
        float distFuncFloor(vec3 p){
            return dot(p, vec3(0.0, 1.0, 0.0)) + 1.0;
        }

        // box distance function
        float distFuncBox(vec3 p){
            return length(max(abs(p) - vec3(2.0, 0.1, 0.5), 0.0)) - 0.1;
        }

        // torus distance function
        float distFuncTorus(vec3 p, vec2 r){
          vec2 d = vec2(length(p.xy) - r.x, p.z);
          return length(d) - r.y;
        }

        // cylinder
        float distFuncCylinder(vec3 p, vec2 r){
            vec2 d = abs(vec2(length(p.xy), p.z)) - r;
            return min(max(d.x, d.y), 0.0) + length(max(d, 0.0)) - 0.1;
        }

        // distance function
        float distFunc(vec3 p){
            vec3 q = rotate(p, radians(time * 10.0), vec3(1.0, 0.5, 0.0));
            float d1 = distFuncTorus(q, vec2(1.5, 0.25));
            float d2 = distFuncBox(q);
            float d3 = distFuncCylinder(q, vec2(0.75, 0.25));
            return smoothMin(smoothMin(d1, d2, 16.0), d3, 16.0);
        }

        vec3 getNormal(vec3 p){
            float d = 0.0001;
            return normalize(vec3(
                distFunc(p + vec3(  d, 0.0, 0.0)) - distFunc(p + vec3( -d, 0.0, 0.0)),
                distFunc(p + vec3(0.0,   d, 0.0)) - distFunc(p + vec3(0.0,  -d, 0.0)),
                distFunc(p + vec3(0.0, 0.0,   d)) - distFunc(p + vec3(0.0, 0.0,  -d))
            ));
        }

        void main(void) {
            // fragment position
            vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

            // camera
            vec3 cPos = vec3(0.0,  0.0,  3.0); // カメラの位置

            // ray
            vec3 ray = normalize(vec3(sin(fov) * p.x, sin(fov) * p.y, -cos(fov)));

            // marching loop
            vec3  dPos = cPos;    // レイの先端位置
            for (int i = 0; i < 256; i++) {
                float distance = distFunc(dPos);
                dPos +=  ray * distance;
            }

            // hit check
            if (distFunc(dPos) < 0.001) {
                vec3 normal = getNormal(dPos);
                float diff = clamp(dot(lightDir, normal), 0.1, 1.0);
                gl_FragColor = vec4(vec3(diff), 1.0);
            }
        }
    |]
