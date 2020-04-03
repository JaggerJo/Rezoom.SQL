// Learn more about F# at http://fsharp.org

open Rezoom.SQL
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations
open Rezoom.SQL.Asynchronous

type MyModel = SQLModel<".">

module ManagingCategories =
    [<AutoOpen>]
    module Common =
        // ------------------------------------------------------
        //         Validation
        // ------------------------------------------------------
        
        type GuidError = InvalidGuid of string
        
        let validateGuid (x: string): Result<System.Guid, GuidError> =
            match System.Guid.TryParse x with
            | true, guid -> Ok guid
            | false, _ -> Error (InvalidGuid x) 
        
        [<RequireQualifiedAccess>]
        type StringError =
            | MaxLengthExceeded of MaxLength * ActualLength
            | NotBlank
        
        and MaxLength = int
        
        and ActualLength = int
        
        [<RequireQualifiedAccess>]
        type StringConstraint =
            | NotBlank
            | MaxLength of MaxLength
        
        let checkStringConstraint (x: string) (cnstrnt: StringConstraint): Result<string, StringError> =
            match cnstrnt with
            | StringConstraint.NotBlank ->
                if System.String.IsNullOrEmpty x
                    then Error StringError.NotBlank
                    else Ok x
            | StringConstraint.MaxLength max ->
                if x.Length > max
                    then Error (StringError.MaxLengthExceeded (max, x.Length))
                    else Ok x
        
        let validateString (constraints: StringConstraint list) (x: string): Result<string, StringError list> =
            let mutable errors = []
            
            for c in constraints do
                match checkStringConstraint x c with
                | Ok _ -> ()
                | Error e -> errors <- List.append errors [ e ]
            
            if errors.Length > 0
                then Error errors
                else Ok x

    module DataAccess =
        type private InsertComment = SQL<"insert into Categories(Id, Name) values (@id, @name)">
        
        let insertCategory (id: System.Guid) (name: string) =
            async {
                use context = new ConnectionContext()
                do! InsertComment.Command(id = id, name = name).Execute(context) |> Async.AwaitTask    
            }
             
    module Operations =
        // ------------------------------------------------------
        //         Creating a category
        // ------------------------------------------------------
        
        type UpsertCategory = CategoryForm -> Async<UpsertCategoryResult>
        
        and CategoryForm =
            { Id: string
              Name: string }
        
        and UpsertCategoryResult = Result<unit, UpsertCategoryError>
        
        and UpsertCategoryError =
            | CreateValidationError of CategoryFormValidationError list
        
        and CategoryFormValidationError =
            | Id of GuidError
            | Name of StringError list
        
        // ------------------------------------------------------
        //         Deleting a category
        // ------------------------------------------------------
        
        type DeleteCategory = string -> Async<DeleteCategoryResult>
        
        and DeleteCategoryResult = Result<unit, DeleteCategoryError>
        
        and DeleteCategoryError =
            | InvalidCategoryId of GuidError
            | CategoryNotFound
            | CategoryInUseByDefects
        
        let validate (form: CategoryForm): Result<(System.Guid * string), CategoryFormValidationError list> =
            let validatedId =
                form.Id
                |> validateGuid
                |> Result.mapError Id
                
            let validatedName =
                form.Name
                |> validateString [ StringConstraint.MaxLength 100; StringConstraint.NotBlank ]
                |> Result.mapError Name
                
            match validatedId, validatedName with
            | Error e1, Error e2 ->
                Error [ e1; e2 ]
            | Error e, _ | _, Error e ->
                Error [ e ]
            | Ok id, Ok name ->
                Ok (id, name)
        
        let upsertCategory: UpsertCategory =
            fun form ->
                match validate form with
                | Ok (id, name) ->
                    async {
                        do! DataAccess.insertCategory id name
                        return Ok ()
                    }
                | Error validationErrors ->
                    async {
                        return Error (CreateValidationError validationErrors)
                    }

    module Queries =
        type ListUsers = SQL<"""select * from Categories""">
        
        let doListUsers _ =
            async {
                use context = new ConnectionContext()
                return!
                    ListUsers.Command().Execute(context) |> Async.AwaitTask
            }
            
let migrate _ =
    // customize the default migration config so that it outputs a message after running a migration
    let config =
        { MigrationConfig.Default with
            LogMigrationRan = fun m -> printfn "Ran migration: %s" m.MigrationName
        }
    // run the migrations, creating the database if it doesn't exist
    MyModel.Migrate(config)

[<EntryPoint>]
let main argv =
    DefaultConnectionProvider.SetConfigurationReader (fun connectionName ->
        match connectionName.Equals "rzsql" with
        | true -> { ConnectionString = "Data Source=localhost;Initial Catalog=Rezoom;User ID=XXX;Password=XXX"; ProviderName = "System.Data.SqlClient" }
        | false -> failwithf "unknown connection name '%s'" connectionName
    )
    
    migrate ()
    
    async {
        let! result = ManagingCategories.Operations.upsertCategory { Id = "E89B1211-C380-4743-8E07-EC30DA2F920D"; Name = "foo" }
        printfn "%A" result
    } |> Async.RunSynchronously
    
    async {
        let! users = ManagingCategories.Queries.doListUsers ()
        
        for user in users do
            printfn "%s: %s" (string user.Id) (user.Name)
    } |> Async.RunSynchronously
    
    0 // return an integer exit code
