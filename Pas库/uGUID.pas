unit uGUID;

interface
uses
   ActiveX, ComObj;

   function GetGUID: string;
implementation

{ GUID get the unique GUID}
function GetGUID: string;
var
  id: TGUID;
begin
  if  CoCreateGuid(id) = s_ok then
    Result := GUIDToString(id);
end;

end.
