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
    //把FInstance转由各子类自己来保存
    //才能保证在继承TSingleton时，不会出现多个子类用的都是TSingleton里的实例
    case ARequest of
        srAccess: Result := FInstance;
        srCreate: FInstance := CreateInstance;
        srNil     : FInstance := nil;
    end;
end;
class function TFooA.Instance: TFooA;
begin
    //子类不一定要重新引入这个方法，只是为了方便使用，不用每次都转型而已
    Result := TFooA(inherited Instance);
end;
procedure TFooA.StaticMethod;
begin
    ShowMessage('A的静态方法');
end;
procedure TFooA.VirtualMethod;
begin
    ShowMessage('A的虚拟方法');
end;
class function TFooB.AccessInstance(ARequest: TSingletonRequest): TSingleton;
const
    {$J+}FInstance: TSingleton = nil;{$J-}
begin
    //如果TFooB不覆盖这个方法，那么TFooB和TFooA就会共用TFooA的Instance了
    //此外可见，任何TSingleton继承类的AccessInstance的写法都是一样的，非常方便
    case ARequest of
        srAccess: Result := FInstance;
        srCreate: FInstance := CreateInstance;
        srNil     : FInstance := nil;
    end;
end;
procedure TFooB.VirtualMethod;
begin
    ShowMessage('B的虚拟方法');
    inherited;
end;
initialization
finalization  
  TFooA.ReleaseInstance; //记得添加这两行
  TFooB.ReleaseInstance;
end.

