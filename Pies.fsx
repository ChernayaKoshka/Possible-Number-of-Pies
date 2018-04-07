type RecipeIngredient =
    {
        Name: string
        Amount: int
    }
    static member (-) (a,b) =  {a with Amount = a.Amount - b.Amount}

type Pie = 
    {
        PieName: string
        IngredientsNeeded: RecipeIngredient list
    }
    static member (-) (ingredients,pie) =
                    ingredients
                    |> List.map (fun ingredient ->
                            match 
                                pie.IngredientsNeeded
                                |> List.tryFind (fun needed ->
                                    needed.Name = ingredient.Name) with
                            | Some needed -> ingredient-needed
                            | _ -> ingredient)

let rec MaxBakeable pie (ingredientsAvailable:RecipeIngredient list) numberMade =
    let remaining = ingredientsAvailable - pie
    if remaining |> List.exists (fun i -> i.Amount < 0) then
        numberMade
    else
        MaxBakeable pie remaining (numberMade+1)

let rec BakeMaxPies (pies:Pie list) ingredients soFar =
    match pies with
    | pie :: toBake ->
        let remaining = ingredients - pie
        if remaining |> List.exists (fun i -> i.Amount < 0) then
            (ingredients,soFar)
        else
            BakeMaxPies toBake remaining (pie :: soFar)
    | [] ->
        (ingredients,soFar)

let MaxmizePies (pies:Pie list) (ingredientsAvailable:RecipeIngredient list) =
    let maxOfEach = 
        pies
        |> List.collect (fun pie ->
            let count = MaxBakeable pie ingredientsAvailable 0
            List.replicate count pie)
    [for n in 0..maxOfEach.Length ->
        maxOfEach |> List.permute (fun index -> (index + n) % maxOfEach.Length)]
    |> List.distinct
    |> List.map (fun pies -> BakeMaxPies pies ingredientsAvailable [])
    |> List.sortByDescending (fun (ingredients,_) -> (ingredients|>List.sumBy(fun i->i.Amount)))
    |> List.sortByDescending (fun (_,pies) -> (pies.Length))
    |> List.head

let PrintPies (ingredients,pies) =
    printf "You can make: "
    pies
    |> List.iter (fun pie -> printf "%s " pie.PieName)
    printfn ""
    printfn "With:"
    ingredients
    |> List.iter (fun ingredient -> printfn "%d of %s" ingredient.Amount ingredient.Name)
    printfn "Left over"

let test() =
    let pumpkinPie = 
        {PieName="Pumpkin";
        IngredientsNeeded=
            [
                {Name="Scoop of Synthetic Pumpkin Flavoring";Amount=1}
                {Name="Eggs";Amount=3}
                {Name="Cups of Milk";Amount=4}
                {Name="Cups of Sugar";Amount=3}
            ]}

    let applePie = 
        {PieName="Apple";
        IngredientsNeeded=
            [
                {Name="Apple";Amount=1}
                {Name="Eggs";Amount=4}
                {Name="Cups of Milk";Amount=3}
                {Name="Cups of Sugar";Amount=2}
            ]}

    let pies = [pumpkinPie;applePie]

    let maxiPrint ingredients =
        printfn ""
        MaxmizePies pies ingredients |> PrintPies
    
    let availableIngredients = 
        [
            {Name="Scoop of Synthetic Pumpkin Flavoring";Amount=10}
            {Name="Apple";Amount=14}
            {Name="Eggs";Amount=10}
            {Name="Cups of Milk";Amount=42}
            {Name="Cups of Sugar";Amount=24}
        ]
    maxiPrint availableIngredients

    let availableIngredients = 
        [
            {Name="Scoop of Synthetic Pumpkin Flavoring";Amount=12}
            {Name="Apple";Amount=4}
            {Name="Eggs";Amount=40}
            {Name="Cups of Milk";Amount=30}
            {Name="Cups of Sugar";Amount=40}
        ]
    maxiPrint availableIngredients

    let availableIngredients = 
        [
            {Name="Scoop of Synthetic Pumpkin Flavoring";Amount=12}
            {Name="Apple";Amount=14}
            {Name="Eggs";Amount=20}
            {Name="Cups of Milk";Amount=42}
            {Name="Cups of Sugar";Amount=24}
        ]
    maxiPrint availableIngredients
