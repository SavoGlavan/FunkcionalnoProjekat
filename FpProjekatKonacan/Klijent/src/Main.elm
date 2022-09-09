module Main exposing (..)

import Browser
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html exposing (..)
import Http exposing (Error(..), emptyBody, header, jsonBody, stringBody)
import Json.Decode as JD
import Element.Region exposing (description)
import Http exposing (Error(..))
import Svg as S
import Svg.Attributes as SA
import Browser.Events exposing (onKeyPress)
import Html.Events exposing (..)
import Html.Attributes exposing (value)
import Html.Attributes exposing (dir)
import Json.Encode as JE
import Html.Attributes exposing (title)
import Html.Attributes exposing (style)

type alias Model =
    { searchText : String
    , results : List Movie
    , errorMessage: Maybe String
    , loading : Bool
    , genreId:Int
    , star1:String
    , star2:String
    , star3:String
    , director:String
    , link:String
    , title:String
    , imdb:Float
    , titles:String
    , year:Int
    , movieId:Int
    , currentCmd : Cmd Msg
    }

type alias Movie={
    title : String
    , link : String
    , director: String
    , imdb: Float
    , stars:String
    , genre:Int
    , id:Int
    ,year:Int
    }


type Msg
    = 
    MsgGotResults (Result Http.Error (List Movie))
    | MsgInputError
    | MsgChooseMovieId String
    | MsgAddMovie
    | MsgDeleteMovie
    | MsgInputImdbField String
    | MsgInputTitleField String
    | MsgInputLinkField String
    | MsgInputDirectorField String
    | MsgInputStar1Field String
    | MsgInputStar2Field String
    | MsgInputStar3Field String
    | MsgInputYearField String
    | MsgGetMoviesAll
    | MsgGetMoviesAction
    | MsgGetMoviesCrime 
    | MsgGetMoviesDrama
    | MsgGetMoviesAdventure 
    | MsgGetMoviesBiography 
    | MsgGetMoviesHistory 
    | MsgGetMoviesSciFi
    | MsgGetMoviesRomance 
    | MsgGetMoviesWestern 
    | MsgGetMoviesFantasy 
    | MsgGetMoviesComedy
    | MsgGetMoviesThriller
    | MsgChooseGenreId String
    | MsgSuccesfulPost (Result Http.Error ())
    | MsgSuccessfulDelete (Result Http.Error ())
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, cmdSearchAll initModel )


initModel : Model
initModel =
    { searchText = ""
    , results = []
    , errorMessage= Nothing
    , loading=False
    , genreId=1
    ,star1=""
    ,star2=""
    ,star3=""
    ,director=""
    ,link=""
    ,title=""
    ,imdb=0.0
    , titles=""
    , year=0
    , movieId=-1
    ,currentCmd=Cmd.none
    }


view : Model -> Html.Html Msg
view model =
    viewLayout model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgGetMoviesAll ->
            ( {model | loading=True, currentCmd=cmdSearchAll model}, cmdSearchAll model)
        MsgGetMoviesAction ->
            ( {model | loading=True, currentCmd=cmdSearchAction model}, cmdSearchAction model)
        MsgGetMoviesCrime ->
            ( {model | loading=True, currentCmd=cmdSearchCrime model}, cmdSearchCrime model)
        MsgGetMoviesDrama ->
            ( {model | loading=True, currentCmd=cmdSearchDrama model}, cmdSearchDrama model)
        MsgGetMoviesAdventure ->
            ( {model | loading=True, currentCmd=cmdSearchAdventure model}, cmdSearchAdventure model)
        MsgGetMoviesBiography ->
            ( {model | loading=True, currentCmd=cmdSearchBiography model}, cmdSearchBiography model)
        MsgGetMoviesHistory ->
            ( {model | loading=True, currentCmd=cmdSearchHistory model}, cmdSearchHistory model)
        MsgGetMoviesSciFi ->
            ( {model | loading=True, currentCmd=cmdSearchSciFi model}, cmdSearchSciFi model)
        MsgGetMoviesRomance ->
            ( {model | loading=True, currentCmd=cmdSearchRomance model}, cmdSearchRomance model)
        MsgGetMoviesWestern ->
            ( {model | loading=True, currentCmd=cmdSearchWestern model}, cmdSearchWestern model)
        MsgGetMoviesFantasy ->
            ( {model | loading=True, currentCmd=cmdSearchFantasy model}, cmdSearchFantasy model)
        MsgGetMoviesComedy ->
            ( {model | loading=True, currentCmd=cmdSearchComedy model}, cmdSearchComedy model)
        MsgGetMoviesThriller ->
            ( {model | loading=True, currentCmd=cmdSearchThriller model}, cmdSearchThriller model)
        MsgChooseGenreId genreId->
            ( {model | loading=False, genreId=Maybe.withDefault 0 (String.toInt(genreId))}, Cmd.none)
        MsgChooseMovieId movieId->
            ( {model | loading=False, movieId=Maybe.withDefault 0 (String.toInt(movieId))}, Cmd.none)
        MsgInputTitleField title->
            ( {model |  title=title}, Cmd.none)
        MsgInputDirectorField director->
            ( {model |  director=director}, Cmd.none)
        MsgInputLinkField link->
            ( {model |  link=link}, Cmd.none)
        MsgInputStar1Field s1->
            ( {model |  star1=s1}, Cmd.none)
        MsgInputStar2Field s2->
            ( {model |  star2=s2}, Cmd.none)
        MsgInputStar3Field s3->
            ( {model |  star3=s3}, Cmd.none)
        MsgInputImdbField imdb->
            ( {model |  imdb=(Maybe.withDefault 0.0 (String.toFloat(imdb)))}, Cmd.none)
        MsgInputYearField year->
            ( {model |  year=(Maybe.withDefault 0 (String.toInt(year)))}, Cmd.none)
        MsgAddMovie->
              ( { model | loading = True }, postMovies model )
        MsgDeleteMovie->
              ( { model | loading = True }, deleteMovie model model.movieId )
        MsgInputError->
              ( { model | loading = False, errorMessage=Just "Greska! Popunite sva polja!" }, deleteMovie model model.movieId )
        MsgSuccesfulPost result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing,director="", year=0, title="", star1="",star2="", star3="",link="",imdb=0 }, model.currentCmd)
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Error! Fill out all fields!"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )
        MsgGotResults result ->
            let
                newmodel= {
                    model | loading= False }
            in
            
            case result of
                Ok data ->
                    ( { newmodel | results = data, errorMessage = Nothing }, Cmd.none )
                Err error ->
                    case error of
                        NetworkError ->
                            ( { newmodel | errorMessage = Just "Network Error" }, Cmd.none )
                        BadUrl _ ->
                            ( { newmodel | errorMessage = Just "Bad URL" }, Cmd.none )
                        Timeout ->
                            ( { newmodel | errorMessage = Just "Timeout" }, Cmd.none )
                        BadStatus _ ->
                            ( { newmodel | errorMessage = Just "Bad status" }, Cmd.none )
                        BadBody reason ->
                            ( { newmodel | errorMessage = Just reason }, Cmd.none )
        
        MsgSuccessfulDelete result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, model.currentCmd)

                --poruka = data, errorMessage = Nothing }, Cmd.none )
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )



subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.none
yellow=E.rgb255 0xff 0xd6 0x00
red=E.rgb255 255 0 0
blue=E.rgb255 0 0 255
viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [
            
            EBG.color yellow
            
        ]
        (E.column [
            E.padding 20,
            E.spacingXY 0 10
        ] [ 
            viewTitle model,viewSearchBar model, viewResults model])



viewDrop : Model -> Html Msg
viewDrop model =
    
    div [
      Html.Attributes.width  200
    ]
        [   
           
            select [onInput MsgChooseGenreId, style "width" "150px", style "height" "45px"]
               [ 
                option [value "1",style "size" "24" ] [text "Action"]  
                , option [value "2" ] [text "Crime"] 
                , option [value "3" ] [text "Drama"] 
                , option [value "4" ] [text "Adventure"] 
                , option [value "5" ] [text "Biography"] 
                , option [value "6" ] [text "History"] 
                , option [value "7" ] [text "Sci-Fi"] 
                , option [value "8" ] [text "Romance"] 
                , option [value "9" ] [text "Western"]
                , option [value "10" ] [text "Fantasy"] 
                , option [value "11" ] [text "Comedy"] 
                , option [value "12" ] [text "Thriller"] 
               ]
        ]

viewTitle : Model -> E.Element Msg
viewTitle model=E.row[
    EF.bold,
    E.htmlAttribute (Html.Attributes.style "marginLeft" "auto"),
     E.htmlAttribute (Html.Attributes.style "marginRight" "auto"),
    EF.italic,
    EF.size 40 ]
    [E.text "Movies"]
viewGenre : Model -> E.Element Msg
viewGenre model=E.row[
    EF.bold
     ]
    [E.text "Genre:"]
viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.column[E.spacingXY 0 10
    ][
    E.wrappedRow[E.spacingXY 10 0]
        [
           
            --viewGenreId model,
             EI.search []
                { onChange = MsgInputTitleField
                , text = model.title
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Title:")
                }
            , EI.search []
                { onChange = MsgInputDirectorField
                , text = model.director
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Director:")
                }
            , EI.search []
                { onChange = MsgInputYearField
                , text = String.fromInt(model.year)
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Year:")
                }
            , EI.search []
                { onChange = MsgInputLinkField
                , text =  model.link 
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Poster Link:")
                }
             ,EI.search []
                { onChange = MsgInputStar1Field
                , text = model.star1
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Star1:")
                }
        ],
    E.wrappedRow[E.spacingXY 10 0][
       
            EI.search []
                { onChange = MsgInputStar2Field
                , text = model.star2
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Star2:")
                }
            , EI.search []
                { onChange = MsgInputStar3Field
                , text = model.star3
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Star3:")
                },
                EI.search []
                { onChange = MsgInputImdbField
                , text = (String.fromFloat(model.imdb))
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Imdb Rating:")
                },
             viewGenre model,  
             E.html (viewDrop model)
             , viewButtonGeneric "Add Movie" MsgAddMovie
            
    ],
    E.wrappedRow[E.spacingXY 10 0, EF.bold][
        E.text "Choose Movie:",
        E.html (viewDeleteMenu model)
        , viewButtonGeneric "Delete Movie" MsgDeleteMovie
    ],
    E.wrappedRow[EF.color (E.rgb255 255 0 0)][
        viewErroMessage model
    ],
    E.wrappedRow [E.spacing 10, E.paddingXY 0 12]
        [ 
       
        viewMoviesAllButton
        ,viewMoviesActionButton
        ,viewMoviesCrimeButton
        ,viewMoviesDramaButton
        ,viewMoviesAdventureButton
        ,viewMoviesBiographyButton
        ,viewMoviesHistoryButton
        ,viewMoviesSciFiButton
        ,viewMoviesRomanceButton
        ,viewMoviesWesternButton
        ,viewMoviesFantasyButton
        ,viewMoviesComedyButton
        ,viewMoviesThrillerButton
        ,if model.loading then
            E.html loadingImage
            else E.none

        ]
    ]
viewGenreId : Model -> E.Element msg
viewGenreId  model= 
        
            E.text (String.fromInt(model.genreId))
       
            
viewErroMessage : Model -> E.Element msg
viewErroMessage model= 
    case model.errorMessage of
        Just errorMessage->
            E.text errorMessage
        Nothing->
            E.none
            
loadingImage : Html.Html msg
loadingImage=
    
    S.svg[
        SA.width "64px",
        SA.height "64px",
        SA.viewBox "0 0 48 48"
    ]
    [
        S.circle [
            SA.cx "24"
            , SA.cy "24"
            , SA.stroke "#6699AA"
            , SA.strokeWidth "4"
            , SA.r "8"
            , SA.fill "none"
        ]
        [
            S.animate[
                SA.attributeName "opacity",
                SA.values "0;.8;0",
                SA.dur "2s",
                SA.repeatCount "indefinite"
            ][]
        ]
    ]

viewResults : Model -> E.Element msg
viewResults model =
   E.wrappedRow[
        E.spacing 5,
        E.centerX
   ](
   List.map viewMovie model.results
   )
createDeleteOption: Movie->Html msg
createDeleteOption movie =
    (option [value (String.fromInt(movie.id))] [text movie.title])
viewDeleteMenu: Model -> Html Msg
viewDeleteMenu model= 
    div [
       Html.Attributes.width  200
    ]
        [   
           
            select [onInput MsgChooseMovieId ,style "width" "150px", style "height" "45px"]
               (
                    List.map createDeleteOption model.results
               )
        ]
viewMovie : Movie -> E.Element msg
viewMovie movie= 
    E.row [
        E.paddingEach {top=10 ,right=0 ,bottom=0, left=10},
        EBG.color (E.rgb255 0 0 0),
        EB.rounded 15,
        E.centerY
    ][
         viewMoviePoster movie.link movie.title ,
        E.column[
        E.paddingEach {top=0 ,right=0 ,bottom=0, left=10},
        E.width (E.px 300)
        ,E.height (E.px 160)
    ][
        E.paragraph [EF.bold,EF.color yellow][E.text (movie.title++"("++String.fromInt(movie.year)++")")],
        E.paragraph [EF.color yellow][ E.text ("Director: "++movie.director)],
        E.paragraph [EF.color yellow][ E.text ("Stars:" ++movie.stars)],
        E.paragraph [EF.color yellow][ E.text ("Imdb Score: "++String.fromFloat(movie.imdb))],
        E.paragraph [EF.color yellow][ E.text ("Genre: "++(parseGenre movie.genre))]
    ]
    ]

parseGenre : Int -> String
parseGenre id=
    case id of
        1->
            "Action"
        2->
            "Crime"
        3->
            "Drama"
        4->
            "Adventure"
        5->
            "Biography"
        6->
            "History"
        7->
            "Sci-Fi"
        8->
            "Romance"
        9->
            "Western"
        10->
            "Fantasy"
        11->
            "Comedy"
        12->
            "Thriller"
        
        
        _ ->"Error"
viewMoviePoster : String -> String-> E.Element msg
viewMoviePoster link title =
    E.image[
        E.height(E.px 140)
        
    ]{
        src = link,
        description = title 
    }

viewSearchButton : E.Element Msg
viewSearchButton =
    EI.button
        [ EBG.color (E.rgb255 0x00 0x33 0x66)
        , EF.color (E.rgb255 0xEE 0xEE 0xEE)
        , EB.rounded 5
        , E.padding 12
        , E.mouseOver
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        ]
        { onPress = Just MsgGetMoviesAll
        , label = E.text "Search"
        }
viewMoviesAllButton : E.Element Msg
viewMoviesAllButton =
    viewButtonGeneric "All" MsgGetMoviesAll
viewMoviesActionButton : E.Element Msg
viewMoviesActionButton =
    viewButtonGeneric "Action" MsgGetMoviesAction
viewMoviesCrimeButton : E.Element Msg
viewMoviesCrimeButton =
    viewButtonGeneric "Crime" MsgGetMoviesCrime
viewMoviesDramaButton : E.Element Msg
viewMoviesDramaButton =
    viewButtonGeneric "Drama" MsgGetMoviesDrama
viewMoviesAdventureButton : E.Element Msg
viewMoviesAdventureButton =
    viewButtonGeneric "Adventure" MsgGetMoviesAdventure
viewMoviesBiographyButton : E.Element Msg
viewMoviesBiographyButton =
    viewButtonGeneric "Biography" MsgGetMoviesBiography
viewMoviesHistoryButton : E.Element Msg
viewMoviesHistoryButton =
    viewButtonGeneric "History" MsgGetMoviesHistory
viewMoviesSciFiButton : E.Element Msg
viewMoviesSciFiButton =
    viewButtonGeneric "Sci-Fi" MsgGetMoviesSciFi
viewMoviesRomanceButton : E.Element Msg
viewMoviesRomanceButton =
    viewButtonGeneric "Romance" MsgGetMoviesRomance
viewMoviesWesternButton : E.Element Msg
viewMoviesWesternButton =
    viewButtonGeneric "Western" MsgGetMoviesWestern
viewMoviesFantasyButton : E.Element Msg
viewMoviesFantasyButton =
    viewButtonGeneric "Fantasy" MsgGetMoviesFantasy
viewMoviesComedyButton : E.Element Msg
viewMoviesComedyButton =
    viewButtonGeneric "Comedy" MsgGetMoviesComedy
viewMoviesThrillerButton : E.Element Msg
viewMoviesThrillerButton =
    viewButtonGeneric "Thriller" MsgGetMoviesThriller

postMovies : Model -> Cmd Msg
postMovies model =
    if validation model then
    Http.post
        { url = "http://localhost:5000/movies"
        , body = jsonBody (encode model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        }
    else Http.post
        { url = "http://localhost:5000/movies"
        , body = jsonBody(JE.null)
        , expect = Http.expectWhatever MsgSuccesfulPost
        }
validation : Model -> Bool
validation model=
    if model.director=="" then False
    else if model.year==0 then False
    else if model.star1=="" then False
    else if model.star2=="" then False
    else if model.star3=="" then False
    else if model.imdb==0 then False
    else if model.link=="" then False
    else True
deleteMovie : Model ->Int-> Cmd Msg
deleteMovie model id=
    Http.post
        { url = "http://localhost:5000/movies/"++String.fromInt(id)
        , body = emptyBody
        , expect = Http.expectWhatever MsgSuccessfulDelete 
        }

cmdSearchAll : Model -> Cmd Msg
cmdSearchAll model =
    Http.get {
        url = " http://localhost:5000/movies/All" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchAction : Model -> Cmd Msg
cmdSearchAction model =
    Http.get {
        url = " http://localhost:5000/movies/Action" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchCrime : Model -> Cmd Msg
cmdSearchCrime model =
    Http.get {
        url = " http://localhost:5000/movies/Crime" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchDrama : Model -> Cmd Msg
cmdSearchDrama model =
    Http.get {
        url = " http://localhost:5000/movies/Drama" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchAdventure : Model -> Cmd Msg
cmdSearchAdventure model =
    Http.get {
        url = " http://localhost:5000/movies/Adventure" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchBiography : Model -> Cmd Msg
cmdSearchBiography model =
    Http.get {
        url = " http://localhost:5000/movies/Biography" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchHistory : Model -> Cmd Msg
cmdSearchHistory model =
    Http.get {
        url = " http://localhost:5000/movies/History" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchSciFi : Model -> Cmd Msg
cmdSearchSciFi model =
    Http.get {
        url = " http://localhost:5000/movies/Sci-fi" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchRomance : Model -> Cmd Msg
cmdSearchRomance model =
    Http.get {
        url = " http://localhost:5000/movies/Romance" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchWestern : Model -> Cmd Msg
cmdSearchWestern model =
    Http.get {
        url = " http://localhost:5000/movies/Western" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchFantasy : Model -> Cmd Msg
cmdSearchFantasy model =
    Http.get {
        url = " http://localhost:5000/movies/Fantasy" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchComedy: Model -> Cmd Msg
cmdSearchComedy model =
    Http.get {
        url = " http://localhost:5000/movies/Comedy" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
cmdSearchThriller : Model -> Cmd Msg
cmdSearchThriller model =
    Http.get {
        url = " http://localhost:5000/movies/Thriller" 
        , expect = Http.expectJson MsgGotResults decodeMovies
    }
encode : Model -> JE.Value
encode model =
    JE.object
        [   
        ("Poster_Link", JE.string model.link)
        , ( "Series_Title", JE.string model.title )
        , ( "Released_Year", JE.int model.year )
        , ( "Genre", JE.int  model.genreId )
        , ( "IMDB_Rating", JE.float model.imdb )
        , ( "Director", JE.string model.director )
        , ( "Stars", JE.string (model.star1++", "++model.star2++", "++model.star3 ))
        
        
        ]
decodeImageLinks : JD.Decoder String
decodeImageLinks =
    JD.field "thumbnail" JD.string

decodeMovies : JD.Decoder (List Movie)
decodeMovies =
    JD.list decodeMovie


decodeMovie : JD.Decoder Movie
decodeMovie =
    JD.map8 Movie
        (JD.field "Series_Title" JD.string)
        (JD.field "Poster_Link" JD.string)
        (JD.field "Director" JD.string)
        (JD.field "IMDB_Rating" JD.float)
        (JD.field "Stars" JD.string)
        (JD.field "GenreId" JD.int)
        (JD.field "id" JD.int)
        (JD.field "Released_Year" JD.int)
viewButtonGeneric : String -> Msg -> E.Element Msg
viewButtonGeneric naziv msg =
    EI.button
        [ EBG.color (E.rgb255 0 0 0)
        , EF.color yellow
        , EB.rounded 5
        , E.padding 12
        , E.mouseOver
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        ]
        { onPress = Just msg
        , label = E.text naziv
        }