{$mode objfpc}{$H+}
unit events;

interface

type
  // Event-Typen für Mini-LCL
  TNotifyEvent = procedure(Sender: TObject) of object;
  
  // TODO: Weitere Event-Typen können hier hinzugefügt werden
  // TMouseEvent = procedure(Sender: TObject; Button: TMouseButton; X, Y: Integer) of object;
  // TKeyEvent = procedure(Sender: TObject; var Key: Word) of object;

implementation

end.