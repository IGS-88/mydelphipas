unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSelector, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure WMMoving(var Message: TWMMoving); message WM_MOVING;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
                                                           
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  b: TSelector;
implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  b := TSelector.Create;
//  b.Parent := Self;
  b.Show;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  b.SetPosition(Point(50,100),400,200);
//  b.Parent := Self;
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  r: TRect;
begin
  r := b.GetClient;
  r.TopLeft := ScreenToClient(r.TopLeft);
  r.BottomRight := ScreenToClient(r.BottomRight);
  ShowMessage('l:'+IntToStr(r.Left)+' r:'+inttostr(r.Right)+' t:'+IntToStr(r.Top)+' b:'+IntToStr(r.Bottom));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  SetWindowLong(application.handle,GWL_ExSTYLE,WS_EX_TOOLWINDOW);
end;

procedure TForm1.WMMove(var Message: TWMMove);
begin
  Caption := '1';
end;

procedure TForm1.WMMoving(var Message: TWMMoving);
begin
  Caption := IntToStr(Left);
end;

procedure TForm1.WMNCHitTest(var Message: TWMNCHitTest);
begin
//  Message.Result := HTCAPTION;
  inherited;
end;

procedure TForm1.WMSize(var Message: TWMSize);
begin
  Caption := '555';
end;

end.
