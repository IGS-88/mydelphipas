unit SingletonSubClassExamples;

interface
uses
  Singleton, Dialogs;
type
    TFooA = class(TSingleton)
    protected
        class function AccessInstance(ARequest: TSingletonRequest): TSingleton; override;
    public
        class function Instance: TFooA; reintroduce;
        procedure StaticMethod;
        procedure VirtualMethod; virtual;
    end;
    TFooB = class(TFooA)
    protected
        class function AccessInstance(ARequest: TSingletonRequest): TSingleton; override;
    public
        procedure VirtualMethod; override;
    end;
implementation
class function TFooA.AccessInstance(ARequest: TSingletonRequest): TSingleton; 
const
    {$J+}FInstance: TSingleton = nil;{$J-}
begin
    //��FInstanceת�ɸ������Լ�������
    //���ܱ�֤�ڼ̳�TSingletonʱ��������ֶ�������õĶ���TSingleton���ʵ��
    case ARequest of
        srAccess: Result := FInstance;
        srCreate: FInstance := CreateInstance;
        srNil     : FInstance := nil;
    end;
end;
class function TFooA.Instance: TFooA;
begin
    //���಻һ��Ҫ�����������������ֻ��Ϊ�˷���ʹ�ã�����ÿ�ζ�ת�Ͷ���
    Result := TFooA(inherited Instance);
end;
procedure TFooA.StaticMethod;
begin
    ShowMessage('A�ľ�̬����');
end;
procedure TFooA.VirtualMethod;
begin
    ShowMessage('A�����ⷽ��');
end;
class function TFooB.AccessInstance(ARequest: TSingletonRequest): TSingleton;
const
    {$J+}FInstance: TSingleton = nil;{$J-}
begin
    //���TFooB�����������������ôTFooB��TFooA�ͻṲ��TFooA��Instance��
    //����ɼ����κ�TSingleton�̳����AccessInstance��д������һ���ģ��ǳ�����
    case ARequest of
        srAccess: Result := FInstance;
        srCreate: FInstance := CreateInstance;
        srNil     : FInstance := nil;
    end;
end;
procedure TFooB.VirtualMethod;
begin
    ShowMessage('B�����ⷽ��');
    inherited;
end;
initialization
finalization  
  TFooA.ReleaseInstance; //�ǵ����������
  TFooB.ReleaseInstance;
end.

