unit Singleton;

interface

uses
  Windows, SysUtils, Classes;

type
  TSingletonRequest = (srAccess, srCreate, srNil);

type
  TSingleton = class(TPersistent)
  private
    FOnFinalize: TNotifyEvent;
    FOnInitialize: TNotifyEvent;
    class function InstanceAccessProxy(ARequest: TSingletonRequest): TSingleton;
  protected
    constructor CreateInstance; virtual;
    class function AccessInstance(ARequest: TSingletonRequest): TSingleton; virtual; abstract;
    procedure DoFinalize; virtual;
    procedure DoInitialize; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function Instance: TSingleton;
    class procedure ReleaseInstance;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property OnFinalize: TNotifyEvent read FOnFinalize write FOnFinalize;
  end;

type
  TSingletonClass = class of TSingleton;

implementation

constructor TSingleton.Create;
begin
  //��������Create��������������ʵ��
  raise Exception.CreateFmt('����Instance�����ʵ�����%s.', [ClassName]);
end;

destructor TSingleton.Destroy;
begin
  if InstanceAccessProxy(srAccess) = Self then InstanceAccessProxy(srNil);
  inherited Destroy;
end;

constructor TSingleton.CreateInstance;
begin
  if ClassType = TSingleton then
    inherited Create
  else
    inherited;
end;
class function TSingleton.InstanceAccessProxy(ARequest: TSingletonRequest): TSingleton;
var
  hMutex: THandle;
begin
  case ARequest of
    srAccess  : ;
    srCreate  : if AccessInstance(srAccess) = nil then
                begin
                  //��ԭ�����ӵĻ����ϲ������̻߳���
                  hMutex := CreateMutex(nil, False, PChar('Singleton mutex of ' + String(Self.ClassName)));
                  WaitForSingleObject(hMutex, INFINITE);
                  if AccessInstance(srAccess) = nil then AccessInstance(srCreate);
                  ReleaseMutex(hMutex);
                end;
    srNil : AccessInstance(srNil);
  else
   raise Exception.Create('�Ƿ���Singleton����!');
  end;
  Result := AccessInstance(srAccess);
end;
class function TSingleton.Instance: TSingleton;
begin
  Result := InstanceAccessProxy(srCreate);
end;
procedure TSingleton.DoFinalize;
begin
  //�����˳�ʼ���¼�
  if Assigned(FOnFinalize) then FOnFinalize(Self);
end;
procedure TSingleton.DoInitialize;
begin
  //�������սữ�¼�
  if Assigned(FOnInitialize) then FOnInitialize(Self);
end;
procedure TSingleton.AfterConstruction;
begin
  inherited;
  DoInitialize;
end;
procedure TSingleton.BeforeDestruction;
begin
  DoFinalize;
  inherited;
end;
class procedure TSingleton.ReleaseInstance;
begin
  InstanceAccessProxy(srAccess).Free;
end;

end.

