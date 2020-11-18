# initials_inflator #freepascal #lazarus #delphi
Linrary provides a functions to inflate russian initials to the any cases with gender reccgnize using different rest api

Interface of library:

    // Returns initials in needed case in simple string
    function GetInitials(Initials: string): CasesResponse;

    // Returns words combination in needed case in simple string
    function GetWordsCase(Words: string): CasesResponse;

    // Returns initials in needed case in simple string with gender in var-parameter
    function GetGenderAndInitials(Initials: string; var Gender: TGender): CaseResponse;
 
 where:
 
  TWordCase = (Nominative, Gentitive, Dative, Accusative, Instrumental, Prepositional);
  TGender = (Male, Female, Unrecognized);
  CasesResponse = array[TWordCase] of string;
