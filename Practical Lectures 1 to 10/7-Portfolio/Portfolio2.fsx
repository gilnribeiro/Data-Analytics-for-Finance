// Last updated 2021-04-29 09:00
#r "nuget: NodaTime"

open System
open NodaTime

type SecurityId =
    | Ticker of string
    | Cusip of string
    | Bloomberg of string
    | Permno of int

type InvestmentUniverse = 
    { FormationMonth : YearMonth 
      Securities : SecurityId array }

type SecuritySignal = 
    { SecurityId : SecurityId
      Signal : float }

type SecuritiesWithSignals =
    { FormationMonth : YearMonth 
      Signals : SecuritySignal array }

type PortfolioId = 
    | Named of string
    | Indexed of name:string * index:int

type AssignedPortfolio =
    { PortfolioId : PortfolioId 
      Signals : SecuritySignal array }





//// Misc
/// 

// Type extension to deal with a YearMonth bug.
// See https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/type-extensions
// to understand type extensions.
type YearMonth with
    // taken from here in the NodaTime repo until the bug that prevents it from being accessible is available
    // https://github.com/carlosschults/nodatime/blob/43e9f24c2ba5a7ed0fd145c082d9e63cd50b1149/src/NodaTime/YearMonth.cs#L156
    member this.PlusMonths(months:int) =
                this.OnDayOfMonth(1).PlusMonths(months).ToYearMonth()