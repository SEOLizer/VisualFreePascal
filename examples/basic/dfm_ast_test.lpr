{$mode objfpc}{$H+}
program dfm_ast_test;

uses
  SysUtils, Classes, dfm_ast;

var
  Doc: TDfmDocument;
  Node: TDfmNode;
  Val: TDfmValue;
begin
  WriteLn('DFM AST Test');
  WriteLn('============');
  WriteLn;
  
  // Teste AST-Datenstrukturen
  Doc := TDfmDocument.Create;
  try
    Node := TDfmNode.Create('Form1', 'TForm');
    Doc.Root := Node;
    
    // Füge Property hinzu
    Val := TDfmValue.Create(dvkInteger, '100');
    Node.Properties.AddObject('Left', Val);
    
    WriteLn('✓ Document erstellt');
    WriteLn('  Root: ', Doc.Root.InstanceName, ': ', Doc.Root.ClassName);
    WriteLn('  Properties: ', Doc.Root.Properties.Count);
    
    if Doc.Root.Properties.Count > 0 then
    begin
      WriteLn('  Left = ', TDfmValue(Doc.Root.Properties.Objects[0]).AsInteger);
    end;
  finally
    Doc.Free;
  end;
  
  WriteLn;
  WriteLn('Test erfolgreich!');
end.
