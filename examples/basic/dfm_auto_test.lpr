{$mode objfpc}{$H+}
program dfm_auto_test;

uses
  SysUtils, Classes, dfm_ast, dfm_parser, dfm_writer;

function TestRoundtrip: Boolean;
var
  Parser1, Parser2: TDfmParser;
  Writer: TDfmWriter;
  Doc1, Doc2: TDfmDocument;
  Input, Middle, Output: string;
  I: Integer;
begin
  WriteLn('=== Roundtrip Test ===');
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 0'#13#10 +
           '  Top = 0'#13#10 +
           '  Width = 640'#13#10 +
           '  Height = 480'#13#10 +
           '  Caption = ''My Application'''#13#10 +
           '  object Button1: TButton'#13#10 +
           '    Left = 10'#13#10 +
           '    Top = 10'#13#10 +
           '    Width = 75'#13#10 +
           '    Height = 25'#13#10 +
           '    Caption = ''OK'''#13#10 +
           '  end'#13#10 +
           'end';
  
  // First parse
  Parser1 := TDfmParser.Create(Input);
  Writer := TDfmWriter.Create;
  try
    Doc1 := Parser1.Parse;
    if not Assigned(Doc1) or not Assigned(Doc1.Root) then
    begin
      WriteLn('✗ First parse failed');
      Result := False;
      Exit;
    end;
    
    WriteLn('✓ First parse successful');
    WriteLn('  Properties: ', Doc1.Root.Properties.Count);
    WriteLn('  Children: ', Doc1.Root.Children.Count);
    
    Middle := Writer.WriteDocument(Doc1);
    
    // Second parse
    Parser2 := TDfmParser.Create(Middle);
    try
      Doc2 := Parser2.Parse;
      if not Assigned(Doc2) or not Assigned(Doc2.Root) then
      begin
        WriteLn('✗ Second parse failed');
        Result := False;
        Exit;
      end;
      
      WriteLn('✓ Second parse successful');
      Output := Writer.WriteDocument(Doc2);
      
      // Vergleiche
      Result := Middle = Output;
      if Result then
        WriteLn('✓ Roundtrip successful!')
      else
        WriteLn('✗ Roundtrip failed');
        
      Doc2.Free;
    finally
      Parser2.Free;
    end;
    
    Doc1.Free;
  finally
    Parser1.Free;
    Writer.Free;
  end;
end;

function TestManualCreation: Boolean;
var
  Doc: TDfmDocument;
  Form, Button: TDfmNode;
  Writer: TDfmWriter;
  Output: string;
  Expected: string;
begin
  WriteLn('=== Manual Creation Test ===');
  
  // Erstelle Dokument manuell
  Doc := TDfmDocument.Create;
  try
    Form := TDfmNode.Create('MainForm', 'TForm');
    Doc.Root := Form;
    
    Form.Properties.AddObject('Left', TDfmValue.Create(dvkInteger, '100'));
    Form.Properties.AddObject('Caption', TDfmValue.Create(dvkString, 'Test'));
    Form.Properties.AddObject('Visible', TDfmValue.Create(dvkBoolean, 'True'));
    
    Button := TDfmNode.Create('BtnOk', 'TButton');
    Button.Properties.AddObject('Left', TDfmValue.Create(dvkInteger, '10'));
    Button.Properties.AddObject('Caption', TDfmValue.Create(dvkString, 'OK'));
    Form.AddChild(Button);
    
    Writer := TDfmWriter.Create;
    try
      Output := Writer.WriteDocument(Doc);
      WriteLn('Generated DFM:');
      WriteLn(Output);
      
      Result := (Pos('object MainForm: TForm', Output) > 0) and
                (Pos('object BtnOk: TButton', Output) > 0);
                
      if Result then
        WriteLn('✓ Manual creation successful!')
      else
        WriteLn('✗ Manual creation failed');
    finally
      Writer.Free;
    end;
  finally
    Doc.Free;
  end;
end;

begin
  WriteLn('DFM Parser - Automated Test');
  WriteLn('===========================');
  WriteLn;
  
  try
    TestRoundtrip;
    WriteLn;
    TestManualCreation;
    WriteLn;
    WriteLn('All tests completed!');
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.
