{$mode objfpc}{$H+}
program dfm_final_test;

uses
  SysUtils, Classes, dfm_ast, dfm_parser, dfm_writer;

procedure TestRoundtrip;
var
  Parser: TDfmParser;
  Writer: TDfmWriter;
  Doc: TDfmDocument;
  Input, Middle, Output: string;
  I, J: Integer;
begin
  WriteLn('=== Roundtrip Test ===');
  WriteLn;
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 0'#13#10 +
           '  Top = 0'#13#10 +
           '  Width = 640'#13#10 +
           '  Height = 480'#13#10 +
           '  Caption = ''My Application'''#13#10 +
           '  Visible = True'#13#10 +
           '  object Button1: TButton'#13#10 +
           '    Left = 10'#13#10 +
           '    Top = 10'#13#10 +
           '    Width = 75'#13#10 +
           '    Height = 25'#13#10 +
           '    Caption = ''OK'''#13#10 +
           '  end'#13#10 +
           '  object Panel1: TPanel'#13#10 +
           '    Left = 100'#13#10 +
           '    Top = 100'#13#10 +
           '    object Label1: TLabel'#13#10 +
           '      Caption = ''Hello'''#13#10 +
           '    end'#13#10 +
           '  end'#13#10 +
           'end';
  
  WriteLn('Original:');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create;
  Writer := TDfmWriter.Create;
  try
    // First parse
    Doc := Parser.ParseText(Input);
    if not Assigned(Doc) or not Assigned(Doc.Root) then
    begin
      WriteLn('✗ First parse failed');
      Exit;
    end;
    
    WriteLn('✓ First parse successful');
    WriteLn('  Properties: ', Doc.Root.Properties.Count);
    WriteLn('  Children: ', Doc.Root.Children.Count);
    
    for J := 0 to Doc.Root.Children.Count - 1 do
      WriteLn('  - ', TDfmNode(Doc.Root.Children[J]).InstanceName);
    
    Middle := Writer.WriteDocument(Doc);
    Doc.Free;
    
    // Second parse
    Doc := Parser.ParseText(Middle);
    if not Assigned(Doc) or not Assigned(Doc.Root) then
    begin
      WriteLn('✗ Second parse failed');
      Exit;
    end;
    
    Output := Writer.WriteDocument(Doc);
    Doc.Free;
    
    // Compare
    WriteLn;
    if Middle = Output then
      WriteLn('✓ Roundtrip successful!')
    else
    begin
      WriteLn('✗ Roundtrip failed');
      WriteLn('Output:');
      WriteLn(Output);
    end;
    
  finally
    Parser.Free;
    Writer.Free;
  end;
end;

procedure TestManualCreation;
var
  Doc: TDfmDocument;
  Form, Button: TDfmNode;
  Writer: TDfmWriter;
  Output: string;
begin
  WriteLn('=== Manual AST Creation ===');
  WriteLn;
  
  Doc := TDfmDocument.Create;
  try
    Form := TDfmNode.Create('MainForm', 'TForm');
    Doc.Root := Form;
    
    Form.Properties.AddObject('Left', TDfmValue.Create(dvkInteger, '100'));
    Form.Properties.AddObject('Top', TDfmValue.Create(dvkInteger, '50'));
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
      
      if (Pos('object MainForm: TForm', Output) > 0) and
         (Pos('object BtnOk: TButton', Output) > 0) then
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
  WriteLn('DFM Parser - Final Test');
  WriteLn('=======================');
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
