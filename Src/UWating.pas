unit UWating;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TWaiting = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Waiting: TWaiting;

implementation

{$R *.dfm}

procedure TWaiting.Timer1Timer(Sender: TObject);
begin
  if  Length(Label1.Caption)<40 then
  begin
    Label1.Caption:= ' '+Label1.Caption;
  end
  else
  Label1.Caption:= 'Wating';


end;

end.
