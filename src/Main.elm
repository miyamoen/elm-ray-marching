module Main exposing (main)

{-
   Rotating triangle, that is a "hello world" of the WebGL
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Html exposing (Html)
import Html.Attributes as HAttrs
import Html.Events
import Json.Decode as JD
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


size : number
size =
    256


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
      , mouse = vec2 0.5 0.5
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Elapse elapsed ->
            ( { model | currentTime = model.currentTime + elapsed }, Cmd.none )

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
        (JD.field "offsetX" JD.float)
        (JD.field "offsetY" JD.float)
        |> JD.map (Vec2.scale (1 / size) >> tagger)



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


fragmentShader : Shader {} Uniforms {}
fragmentShader =
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
