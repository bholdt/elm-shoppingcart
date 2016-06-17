import Html exposing (Html, button, div, text)
import Html.App as Html
import Html.Events exposing (onClick)
import List.Extra

main =
  Html.beginnerProgram { model = initModel, view = view, update = update }

type alias ProductId = Int

type alias Product = {
    id: ProductId
  , name: String
  , price: Float
  , inventory: Int
}

type alias ShoppingCartItem = {
    id: Int
  , name: String
  , price: Float
  , total: Float
  , quantity: Int
}

type alias Model = {
    products: List Product
  , shoppingCart: List ProductId
}

initModel: Model
initModel = {
    products = [  { id = 1, name = "Wine", price = 10.20, inventory = 10}
                , { id = 2, name = "Cheese", price = 5.10, inventory = 10}
                , { id = 3, name = "Bread", price = 1.20, inventory = 5} ]
   , shoppingCart = [] }

type Msg = AddToCart ProductId | Checkout

update msg model =
  case msg of
    AddToCart product ->
      { model | shoppingCart = product :: model.shoppingCart, products = lessInventory product model.products }
    Checkout ->
      model

lessInventory: ProductId -> List Product -> List Product
lessInventory productId products =
  List.Extra.updateIf (\a -> a.id == productId) (\p -> { p | inventory = p.inventory - 1 }) products

addOrRemove product = 
  if product.inventory == 0 then
    text "Out of stock"
  else
    button [ onClick (AddToCart product.id) ] [ text "Add to cart" ]

viewProduct product = 
  div [] [ 
      div [] [ text product.name ]
    , div [] [ text (toString product.price) ]
    , div [] [ addOrRemove product ]
  ]

addQuantity: ShoppingCartItem -> List ShoppingCartItem -> List ShoppingCartItem
addQuantity curr items =
  let
    index = List.Extra.findIndex (\a -> a.id == curr.id) items
  in
    case index of
      Nothing ->
        curr :: items
      Just i ->
        Maybe.withDefault items (List.Extra.updateAt i (\a -> { a | quantity = a.quantity + 1 }) items)

mapToShoppingCart: List ProductId -> List Product -> List ShoppingCartItem
mapToShoppingCart productIds products =
  let
    items = (List.map (\p -> mapToProduct p products) productIds)
  in
    List.foldr addQuantity [] items

mapToProduct: ProductId -> List Product -> ShoppingCartItem
mapToProduct productId products =
  let
    fp = List.head (List.filter (\p -> p.id == productId) products)
    product = Maybe.withDefault ({id = 0, name = "Huh?", price = 0, inventory = 0}) fp
  in
    ({id = product.id, name = product.name, price = product.price, total = 0.0, quantity = 0})

viewShoppingCart model = 
  div [] [
      text (toString model)
    , text " "
    , div [] (List.map (\a -> text (toString a)) (mapToShoppingCart model.shoppingCart model.products))
  ]

view model =
  div [] [
      div []
        (List.map viewProduct model.products)
    , div [] [viewShoppingCart model]
  ]