# initials_inflator #freepascal #lazarus
Library provides functions to inflate russian initials to any case with gender recognize using different rest api

Interface of library:

    // Returns initials in needed case in array of strings with case as index
    function GetInitials(Initials: string): CasesResponse;

    // Returns words combination in needed case in array of strings with case as index
    function GetWordsCase(Words: string): CasesResponse;

    // Returns initials in needed case in simple string with gender in var-parameter
    function GetGenderAndInitials(Initials: string; var Gender: TGender): CaseResponse;
 
 where:
 
  TWordCase = (Nominative, Gentitive, Dative, Accusative, Instrumental, Prepositional);
  
  TGender = (Male, Female, Unrecognized);
  
  CasesResponse = array[TWordCase] of string;

  Use generics for RestApi service.
  
  Implemented 2/3 services:
  1. Pymorphy https://github.com/kmike/pymorphy2 (inflates only separated words without context)
  2. Morphos http://morphos.io/site/api#inflect-name (best way imho)
  3. Dadata not implemented yet (I think best service but it's not free)
