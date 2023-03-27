module Main exposing (main)
import Browser.Navigation as Nav
import Url exposing (Url)
import Route exposing (Route)
import Browser
import Page.ListPosts as ListPosts

import Html exposing (..)





main : Program () ListPosts.Model ListPosts.Msg
main =
    Browser.element
        { init = ListPosts.init
        , view = ListPosts.view
        , update = ListPosts.update
        , subscriptions = \_ -> Sub.none
        }



type alias Model =
    { route : Route
    , page : Page
    ,navKey:Nav.Key
    }


type Page
    = NotFoundPage
    | ListPage ListPosts.Model


-- Extract a route from url  and store it in the route field in the main module--
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )

-- creating type msgs specific for eact module , this one handle listpost module --
type Msg
    = ListPageMsg ListPosts.Msg



initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Posts ->
                    let
                        ( pageModel, pageCmds ) =
                            ListPosts.init
                    in
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Html Msg
view model =
    case model.page of
        NotFoundPage ->
            notFoundView

        ListPage pageModel ->
            ListPosts.view pageModel
                |> Html.map ListPageMsg


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListPosts.update subMsg pageModel
            in
            ( { model | page = ListPage updatedPageModel }
            , Cmd.map ListPageMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )    