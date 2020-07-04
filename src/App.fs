module App

open Elmish
open Elmish.React
open Feliz

type TodoItem =
    { Id: int
      Description: string 
      Completed: bool }

type State =
    { TodoList: TodoItem list
      NewTodo: string }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of int
    | DeleteTodo of int

let init() =
    { TodoList = [ { Id = 1; Description  = "Learn F# "; Completed = false } ]
      NewTodo = "" }

let update (msg: Msg) (state: State): State =
    match msg with
    | SetNewTodo newTodo ->
        { state with NewTodo = newTodo }
    | AddNewTodo when state.NewTodo = "" -> state
    | AddNewTodo ->
        let nextId = 
          1 + (state.TodoList
               |> Seq.map(fun i -> i.Id)
               |> Seq.max)
        let newItem = 
          { Id = nextId
            Description = state.NewTodo
            Completed = false }
        { state with
            TodoList = newItem :: state.TodoList
            NewTodo = "" }
    | ToggleCompleted todoId ->
        { state with 
            TodoList =
              state.TodoList
              |> List.map(fun i ->
                  if i.Id = todoId
                    then { i with Completed = not i.Completed }
                    else i ) }
    | DeleteTodo todoId ->
        { state with 
            TodoList =
              state.TodoList
              |> List.where(fun i -> i.Id <> todoId) }

let appTitle =
    Html.p [
      prop.className "title"
      prop.text "Elmish To-Do List"
    ]

let inputField (state: State) (dispatch: Msg -> unit) = 
    Html.div [
      prop.classes [ "field"; "has-addons" ]
      prop.children [
        Html.div [
          prop.classes [ "control"; "is-expanded" ]
          prop.children [
            Html.input [
              prop.classes [ "input"; "is-medium" ]
              prop.valueOrDefault state.NewTodo
              prop.onChange (SetNewTodo >> dispatch)
            ]
          ]
        ]
        Html.div [
          prop.className "control"
          prop.children [
            Html.button [
              prop.classes [ "button"; "isPrimary"; "is-medium" ]
              prop.onClick (fun _ -> dispatch AddNewTodo)
              prop.children [
                Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
              ]
            ]
          ]
        ]
      ]
    ]

let div (classes: string list) (children: Fable.React.ReactElement list) =
  Html.div [
    prop.classes classes
    prop.children children
  ]

let renderTodo (todo: TodoItem) (dispatch: Msg -> unit) =
  // Html.none
  div [ "box" ] [
    div [ "columns"; "is-mobile"; "is-vcentered" ] [
      div [ "column" ] [
        Html.p [
          prop.className "subtitle"
          prop.text todo.Description
        ]
      ]
      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [ 
          Html.button [
            prop.className [ true, "button"; todo.Completed, "is-success" ]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-check" ] ]
            ]
          ]
          Html.button [
            prop.className [ "button"; "is-danger" ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-times" ] ]
            ]
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      for todo in state.TodoList ->
        renderTodo todo dispatch
    ]
  ]   

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      appTitle
      inputField state dispatch
      todoList state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run