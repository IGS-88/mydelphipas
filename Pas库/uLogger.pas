{*******************************************************}
{                                                       }
{       Logger                                          }
{                                                       }
{       版权所有 (C) 2011 Codeup                        }
{                                                       }
{*******************************************************}

(*
 *  使用单例模式制作的Logger，唯一实例，使用时只需要引用本单元，然后调用实际使用的接口，
 *  不需要手动创建任何实例。使用SetOnLogEvent关联Log事件。
 *  2011-04-08 Add 添加注释说明
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
  ELogger = class(Exception); //创建实例异常

  { 实际使用的接口 }
  function WriteLog(AThreadID: Cardinal; const APntGUID:string; ALogLevel: TLogLevel; const ALogMsg: string): Boolean;overload;
  function WriteLog(AThreadID: Cardinal; ALogLevel: TLogLevel; const ALogMsg: string; const pt: Pointer): Boolean;overload;
  procedure SetOnLogEvent(OnLog: TOnLogEvent);
  function GetOnLogEvent: TOnLogEvent;

implementation

type
  { Logger类，实现Log操作，实例由getInstance首次调用创建 外界不需要访问 }
  TLogger = class(TObject)
  private
    FLogInfo: TLogInfo;   //日志信息实例
    FOnLog: TOnLogEvent;  //日志记录事件
    procedure SetOnLog(value: TOnLogEvent);
    function  ReadOnLog: TOnLogEvent;
    class function createInstance: TLogger; //类方法，创建唯一实例
  protected
  public
    ID: string; //GUID
    constructor create;
    destructor  destroy;override;
    class function getInstance: TLogger;  //获取TLogger的唯一实例，首次调用时创建此实例
    function writeLog(AThreadID: Cardinal;const APntGUID:string; ALogLevel: TLogLevel; const ALogMsg: string; const Pt: Pointer): Boolean;
    property OnLog: TOnLogEvent read ReadOnLog write SetOnLog;
  end;

var
  CS_create: TCriticalSection; //实例创建临界区
  CS_write: TCriticalSection;  //Log记录临界区

{ 单元接口实现 }
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
  raise ELogger.CreateFmt('不能手动创建单例类%s.',[classname]);
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
