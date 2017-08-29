module App exposing (..)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen, last)


-- Model


type Insert
    = Before Int String
    | Last String


type alias Card =
    { id : Int
    , label : String
    , column : String
    }


type alias Model =
    { cards : List Card
    , dragDrop : DragDrop.Model Int Insert
    , newCardName : String
    , columns : List Column
    , addingCard : Bool
    }


type alias Column =
    { id : Int
    , collapsed : Bool
    , name : String
    }


insertBefore : a -> (a -> Bool) -> List a -> List a
insertBefore insert when into =
    case splitWhen when into of
        Just ( before, after ) ->
            before ++ insert :: after

        Nothing ->
            into


insertLast : a -> List a -> List a
insertLast new xs =
    xs ++ [ new ]


addCard : String -> List Card -> List Card
addCard name cards =
    let
        newCard =
            Card (nextCardId cards) name "Leads"
    in
        newCard :: cards


moveCard : Int -> Insert -> List Card -> List Card
moveCard cardIdToMove insert cards =
    let
        ( otherCards, movedCards ) =
            List.partition (\card -> card.id /= cardIdToMove) cards
    in
        case movedCards of
            movedCard :: rest ->
                case insert of
                    Before id column ->
                        insertBefore { movedCard | column = column } (\card -> card.id == id) otherCards

                    Last column ->
                        insertLast { movedCard | column = column } otherCards

            [] ->
                cards


cardIds cards =
    List.map (\x -> x.id) cards


cardsInColumn : List Card -> String -> List Card
cardsInColumn cards column =
    List.filter (\card -> card.column == column) cards


nextCardId : List Card -> Int
nextCardId cards =
    let
        existingIds =
            cardIds cards
    in
        case List.maximum existingIds of
            Just max ->
                max + 1

            Nothing ->
                0



-- To try out performance with many cards, increase this number


nrOfGeneratedCards =
    4


model : Model
model =
    { cards = List.foldr addCard [] ([ "Bob", "Fred", "Suzy" ] ++ (List.repeat nrOfGeneratedCards "Albert"))
    , dragDrop = DragDrop.init
    , newCardName = ""
    , columns = [ { id = 1, name = "Leads", collapsed = False }, { id = 2, name = "First Contact", collapsed = False }, { id = 3, name = "Interested", collapsed = False } ]
    , addingCard = False
    }



-- Update


type Msg
    = DragDropMsg (DragDrop.Msg Int Insert)
    | AddCard
    | ToggleAddCard
    | EnterCardName String
    | ToggleCollapse Int


doDragDrop msg model =
    let
        ( dragModel, result ) =
            DragDrop.update msg model.dragDrop

        dragId =
            DragDrop.getDragId model.dragDrop
    in
        { model
            | dragDrop = dragModel
            , cards =
                case result of
                    Nothing ->
                        model.cards

                    Just ( _, insert ) ->
                        case dragId of
                            Nothing ->
                                model.cards

                            Just dragId ->
                                moveCard dragId insert model.cards
        }
            ! []


update msg model =
    case msg of
        DragDropMsg dragMsg ->
            doDragDrop dragMsg model

        AddCard ->
            ( { model
                | cards = addCard model.newCardName model.cards
                , addingCard = False
                , newCardName = ""
              }
            , Cmd.none
            )

        ToggleAddCard ->
            ( { model | addingCard = not model.addingCard, newCardName = "" }, Cmd.none )

        EnterCardName newName ->
            ( { model | newCardName = newName }, Cmd.none )

        ToggleCollapse colId ->
            let
                newColumns =
                    List.map
                        (\x ->
                            if x.id == colId then
                                { x | collapsed = not x.collapsed }
                            else
                                x
                        )
                        model.columns
            in
                ( { model | columns = newColumns }, Cmd.none )



-- View


cardStyle =
    class "card"


columnStyle =
    class "column"


dropStyle =
    class "dropZone"


inputCardStyle =
    style
        [ ( "margin", "10px" )
        ]


columnsStyle =
    style
        [ ( "display", "flex" )
        ]


instructionStyle =
    style
        [ ( "margin", "10px" )
        ]


columnStyleCollapsed =
    class "collapsed"


dropZone insert =
    div
        (dropStyle :: (DragDrop.droppable DragDropMsg insert))
        []


viewCard card withDropZones =
    let
        draggableAttributes =
            DragDrop.draggable DragDropMsg card.id

        attributes =
            cardStyle :: draggableAttributes

        handleDropZone element =
            if withDropZones then
                (dropZone (Before card.id card.column) :: element :: [])
            else
                [ element ]

        cardElement =
            div attributes [ text card.label ]
    in
        div [] (handleDropZone cardElement)


isOneBeforeTheOther : a -> a -> List a -> Bool
isOneBeforeTheOther one other list =
    case list of
        first :: second :: rest ->
            if first == one && second == other then
                True
            else
                isOneBeforeTheOther one other (second :: rest)

        _ ->
            False


getOneAfterThisOne : List a -> a -> Maybe a
getOneAfterThisOne list thisOne =
    case list of
        first :: second :: rest ->
            if first == thisOne then
                Just second
            else
                getOneAfterThisOne (second :: rest) thisOne

        _ ->
            Nothing


viewCardInput : String -> Html Msg
viewCardInput nameSoFar =
    div [ cardStyle ]
        [ input [ size 14, Events.onInput EnterCardName, placeholder "Name", value nameSoFar ] []
        , div [ class "cardBtns" ]
            [ button [ class "btn btn-secondary btn-sm", Events.onClick ToggleAddCard ] [ text "Cancel" ]
            , button [ class "btn btn-primary btn-sm", Events.onClick AddCard ] [ text "Add" ]
            ]
        ]


viewColumn :
    { cards : List Card
    , column : Column
    , colId : Int
    , dragId : Maybe Int
    , addingCard : Bool
    , newCardName : String
    }
    -> Html Msg
viewColumn { cards, column, colId, dragId, addingCard, newCardName } =
    let
        allCardIds =
            cardIds cards

        isLastCardDragged =
            Maybe.map2 (\draggedId theLastCard -> draggedId == theLastCard.id) dragId (last cards)
                |> Maybe.withDefault False

        isCardBeforeBeingDragged cardId =
            dragId
                |> Maybe.andThen (getOneAfterThisOne allCardIds)
                |> Maybe.map ((==) cardId)
                |> Maybe.withDefault False

        showZones cardId =
            case dragId of
                Just id ->
                    cardId /= id && not (isCardBeforeBeingDragged cardId)

                Nothing ->
                    False

        lastDropZone =
            case dragId of
                Just id ->
                    if isLastCardDragged then
                        []
                    else
                        [ dropZone (Last column.name) ]

                Nothing ->
                    []

        viewCards =
            List.map (\card -> viewCard card (showZones card.id)) cards

        collapseBtn =
            button [ class "btn btn-primary right", Events.onClick (ToggleCollapse column.id) ]
                [ text
                    (if column.collapsed then
                        "+"
                     else
                        "Collapse"
                    )
                ]
    in
        div
            [ if column.collapsed then
                columnStyleCollapsed
              else
                columnStyle
            ]
            ([ p []
                [ collapseBtn
                , h3 [ class "colTitle" ] [ text column.name ]
                ]
             , if colId == 0 then
                div []
                    [ if addingCard then
                        viewCardInput newCardName
                      else
                        button [ class "btn btn-primary addCard", Events.onClick ToggleAddCard ] [ text "Add Card" ]
                    ]
               else
                text ""
             ]
                ++ viewCards
                ++ lastDropZone
            )


view : Model -> Html Msg
view model =
    let
        dragId =
            DragDrop.getDragId model.dragDrop

        columns =
            model.columns
                |> List.indexedMap (,)
                |> List.map
                    (\( colId, column ) ->
                        viewColumn
                            { cards = (cardsInColumn model.cards column.name)
                            , column = column
                            , colId = colId
                            , dragId = dragId
                            , addingCard = model.addingCard
                            , newCardName = model.newCardName
                            }
                    )
                |> div [ columnsStyle ]
    in
        div [] [ columns ]
