unit restjson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, fpjson, fpjsonrtti;

const
  RESTOUT_ERROR = 'REST API service does not return any result';

type

  { TRestApi }

  generic TRestApi<T> = class
  private
    HTTPSender: THTTPSend;
    JSONStreamer: TJSONDeStreamer;
  public
    function JSONfromRestUri(Uri: string): T;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRestApi }

constructor TRestApi.Create;
begin
  HTTPSender := THTTPSend.Create;
  JSONStreamer := TJSONDeStreamer.Create(nil);
end;

function TRestApi.JSONfromRestUri(Uri: string): T;
begin
  HTTPSender.Clear;
  Result := T.Create;
  if not HTTPSender.HTTPMethod('GET', Uri) then raise EInOutError.Create(RESTOUT_ERROR);
  JSONStreamer.JSONToObject(GetJSON(HTTPSender.Document) as TJSONObject, Result);
end;

destructor TRestApi.Destroy;
begin
  FreeAndNil(HTTPSender);
  FreeAndNil(JSONStreamer);
  inherited Destroy;
end;

end.
