{*******************************************************}
{                                                       }
{       Logger                                          }
{                                                       }
{       ��Ȩ���� (C) 2011 Codeup                        }
{                                                       }
{*******************************************************}

(*
 *  ʹ�õ���ģʽ������Logger��Ψһʵ����ʹ��ʱֻ��Ҫ���ñ���Ԫ��Ȼ�����ʵ��ʹ�õĽӿڣ�
 *  ����Ҫ�ֶ������κ�ʵ����ʹ��SetOnLogEvent����Log�¼���
 *  2011-04-08 Add ���ע��˵��
 *)
unit uLogger;

interface
uses
  SyncObjs, SysUtils, Classes, uGUID;
type
  TLogLevel = (llException, llError, llWarning, llInfo, llDebug);
  TLogInfo = record
    ThreadID: Cardinal;
    PntGUID: string;
    LogLevel: TLogLevel;
    LogMsg: String;
    LogPoint: Pointer;
  end;
  TOnLogEvent = procedure(Sender: TObject; const ALogInfo: TLogInfo) of object;
  ELogger = class(Exception); //����ʵ���쳣

  { ʵ��ʹ�õĽӿ� }
  function WriteLog(AThreadID: Cardinal; const APntGUID:string; ALogLevel: TLogLevel; const ALogMsg: string): Boolean;overload;
  function WriteLog(AThreadID: Cardinal; ALogLevel: TLogLevel; const ALogMsg: string; const pt: Pointer): Boolean;overload;
  procedure SetOnLogEvent(OnLog: TOnLogEvent);
  function GetOnLogEvent: TOnLogEvent;

implementation

type
  { Logger�࣬ʵ��Log������ʵ����getInstance�״ε��ô��� ��粻��Ҫ���� }
  TLogger = class(TObject)
  private
    FLogInfo: TLogInfo;   //��־��Ϣʵ��
    FOnLog: TOnLogEvent;  //��־��¼�¼�
    procedure SetOnLog(value: TOnLogEvent);
    function  ReadOnLog: TOnLogEvent;
    class function createInstance: TLogger; //�෽��������Ψһʵ��
  protected
  public
    ID: string; //GUID
    constructor create;
    destructor  destroy;override;
    class function getInstance: TLogger;  //��ȡTLogger��Ψһʵ�����״ε���ʱ������ʵ��
    function writeLog(AThreadID: Cardinal;const APntGUID:string; ALogLevel: TLogLevel; const ALogMsg: string; const Pt: Pointer): Boolean;
    property OnLog: TOnLogEvent read ReadOnLog write SetOnLog;
  end;

var
  CS_create: TCriticalSection; //ʵ�������ٽ���
  CS_write: TCriticalSection;  //Log��¼�ٽ���

{ ��Ԫ�ӿ�ʵ�� }
function WriteLog(AThreadID: Cardinal;const APntGUID:string; ALogLevel: TLogLevel; const ALogMsg: string): Boolean;
begin
  Result := TLogger.getInstance.writeLog(AThreadID, APntGUID, ALogLevel, ALogMsg, nil);
end;

function WriteLog(AThreadID: Cardinal; ALogLevel: TLogLevel; const ALogMsg: string; const pt: Pointer): Boolean;
begin
  Result := TLogger.getInstance.writeLog(AThreadID, '', ALogLevel, ALogMsg, pt);
end;

procedure SetOnLogEvent(OnLog: TOnLogEvent);
begin
  TLogger.getInstance.OnLog := OnLog;
end;

function GetOnLogEvent: TOnLogEvent;
begin
  Result := TLogger.getInstance.OnLog;
end;

{ TLogger }
constructor TLogger.create;
begin
  raise ELogger.CreateFmt('�����ֶ�����������%s.',[classname]);
end;

class function TLogger.createInstance: TLogger;
begin
  Result := inherited NewInstance as TLogger;
  Result.ID := GetGUID;
  Result.FOnLog := nil;
end;

destructor TLogger.destroy;
begin
  { TODO : destroy }
  inherited;
end;

class function TLogger.getInstance: TLogger;
const
  {$J+} Instance: TLogger = nil;{$J-}
begin
  CS_create.Acquire;
  if Instance = nil then
  begin
    Instance := createInstance;
  end;
  Result := Instance;
  CS_create.Leave;
end;

function TLogger.ReadOnLog: TOnLogEvent;
begin
  Result := TLogger.getInstance.FOnLog;
end;

procedure TLogger.SetOnLog(value: TOnLogEvent);
begin
  TLogger.getInstance.FOnLog := value;
end;

function TLogger.writeLog(AThreadID: Cardinal; const APntGUID: string;
  ALogLevel: TLogLevel; const ALogMsg: string; const Pt: Pointer): Boolean;
begin
  with TLogger.getInstance do
  begin
    CS_write.Acquire;
    with FLogInfo do
    begin
      ThreadID := AThreadID;
      PntGUID := APntGUID;
      LogLevel := ALogLevel;
      LogMsg := ALogMsg;
      LogPoint := Pt;
    end;
    if Assigned(FOnLog) then
    begin
      FOnLog(Self, FLogInfo);
    end;
    CS_write.Leave;
  end;
  Result := True;
end;

initialization
  CS_create := TCriticalSection.Create;
  CS_write := TCriticalSection.Create;
finalization
  CS_create.Free;
  CS_write.Free;
end.
