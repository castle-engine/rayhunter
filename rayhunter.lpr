{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "rayhunter".

  "rayhunter" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "rayhunter" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "rayhunter"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

program RayHunter;

uses SysUtils, VectorMath, VRMLRayTracer, VRMLScene, VRMLTriangleOctree,
  Images, KambiUtils, Object3DAsVRML, ProgressUnit, ProgressConsole,
  SpaceFillingCurves, ParseParametersUnit, VRMLNodesDetailOptions, VRMLNodes,
  RaysWindow, KambiStringUtils, VRMLErrors, KambiTimeUtils;

var
  { parametry podawane w linii polecen -------------------------------------- }
  { parametry ktore musza byc podane w linii polecen }
  RTDepth: Integer;
  RTKind: TRaytracerKind;
  PTNonPrimarySamplesCount: Cardinal; { meaningless if RTKind = rtkClassic }
  ImageWidth, ImageHeight: Cardinal;
  SceneFilename, OutImageFilename: string;

  { inne parametry ktore maja jakies defaultowe wartosci i nie musza byc zawsze
    podawane }
  { kolor tla, uzywany jesli BGImageFilename = ''. }
  SceneBGColor: TVector3Single = (0, 0, 0);

  CamPos: TVector3Single;
  CamDir: TVector3Single;
  CamUp: TVector3Single;
  Param_CamPos: TVector3Single;
  Param_CamDir: TVector3Single;
  Param_CamUp: TVector3Single;
  WasParam_CamPos: boolean = false;
  WasParam_CamDir: boolean = false;
  WasParam_CamUp: boolean = false;

  ViewAngleX: Single = 60;
  ForceViewAngleY: Single = 0.0; { 0.0 oznacza ze nie zostalo podane }

  OctreeMaxDepth: integer = DefTriangleOctreeMaxDepth;
  OctreeLeafCapacity: integer = DefTriangleOctreeLeafCapacity;

  WritePartialRows: Cardinal = 0;
  WritePartialRows_LogFile: string = '';

  PTPrimarySamplesCount: Cardinal = 1;
  PTDirectIllumSamplesCount: Cardinal = 1;
  PTRRoulContinue: Single = 0.5;
  FirstRow: Cardinal = 0;

  { helper variables for doing the job --------------------------------------- }
  Scene: TVRMLScene;
  Image: TImage;

procedure PixelsMadeNotify(PixelsMadeCount: Cardinal; Data: Pointer);
begin
 if PixelsMadeCount mod ImageWidth <> 0 then Exit;

 Progress.Step;
 { jezeli WritePartialRows jest wlaczone i jezeli ilosc zrobionych wierszy
   jest podzielna przez WritePartialRows (i wieksza od zera) i jezeli
   nie zrobilismy jeszcze calego obrazka to zapisz obrazek czesciowy. }
 if (WritePartialRows > 0) and (PixelsMadeCount > 0) and
   ((PixelsMadeCount div ImageWidth) mod WritePartialRows = 0) and
    (PixelsMadeCount < ImageWidth * ImageHeight) then
 begin
  try
   SaveImage(Image, OutImageFilename);
  except
    on E: Exception do
    begin
     Writeln(ErrOutput, Format(
       'Warning: Saving partial image to "%s" failed: %s',
       [OutImageFilename, E.Message]));
     { In case of exception while SaveImage, write a warning
       (don't fail with error).
       Exit without writing to WritePartialRows_LogFile. }
     Exit;
    end;
  end;
  StringToFile(WritePartialRows_LogFile,
    IntToStr(PixelsMadeCount div ImageWidth));
 end;
end;

const
  Version = '1.3.1';
  Options: array[0..14]of TOption =
  ( (Short:  #0; Long: 'scene-bg-color'; Argument: oaRequired3Separate),
    (Short: 'p'; Long: 'camera-pos'; Argument: oaRequired3Separate),
    (Short: 'd'; Long: 'camera-dir'; Argument: oaRequired3Separate),
    (Short: 'u'; Long: 'camera-up'; Argument: oaRequired3Separate),
    (Short:  #0; Long: 'view-angle-x'; Argument: oaRequired),
    (Short:  #0; Long: 'force-view-angle-y'; Argument: oaRequired),
    (Short:  #0; Long: 'octree-max-depth'; Argument: oaRequired),
    (Short:  #0; Long: 'octree-leaf-capacity'; Argument: oaRequired),
    (Short:  #0; Long: 'write-partial-rows'; Argument: oaRequired2Separate),
    (Short:  #0; Long: 'direct-illum-samples-count'; Argument: oaRequired),
    (Short:  #0; Long: 'r-roul-continue'; Argument: oaRequired),
    (Short:  #0; Long: 'primary-samples-count'; Argument: oaRequired),
    (Short:  #0; Long: 'first-row'; Argument: oaRequired),
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
   case OptionNum of
    0 : SceneBGColor := SeparateArgsToVector3Single(SeparateArgs);
    1 : begin Param_CamPos := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamPos := true end;
    2 : begin Param_CamDir := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamDir := true end;
    3 : begin Param_CamUp := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamUp := true end;
    4 : ViewAngleX := StrToFloat(Argument);
    5 : ForceViewAngleY := StrToFloat(Argument);
    6 : OctreeMaxDepth := StrToInt(Argument);
    7 : OctreeLeafCapacity := StrToInt(Argument);
    8 : begin
         WritePartialRows := StrToInt(SeparateArgs[1]);
         WritePartialRows_LogFile := SeparateArgs[2];
        end;
    9 : PTDirectIllumSamplesCount := StrToInt(Argument);
    10: PTRRoulContinue := StrToFloat(Argument);
    11: PTPrimarySamplesCount := StrToInt(Argument);
    12: FirstRow := StrToInt(Argument);
    13: begin
         InfoWrite(
          {'0123456789012345678901234567890123456789012345678901234567890123456789012345' }
           'RayHunter: ray-tracer based on VRML 1.0 file format (but handles 3DS too).' +nl+
           'Classic (Whitted-style) ray-tracer (based on local lighting equations' +nl+
           'from VRML 97 specification) and simple Monte Carlo path tracer are' +nl+
           'available.' +nl+
           nl+
           'Usage:' +nl+
           '  rayhunter [OPTIONS]... classic DEPTH IMAGE-SIZE-X IMAGE-SIZE-Y' +nl+
           '    SCENE-FILENAME OUT-IMAGE-FILENAME' +nl+
           'for classic raytracer or' +nl+
           '  rayhunter [OPTIONS]... path DEPTH NON-PRIMARY-SAMPLES-COUNT' +nl+
           '    IMAGE-SIZE-X IMAGE-SIZE-Y SCENE-FILENAME OUT-IMAGE-FILENAME' +nl+
           'for path tracer.' +nl+
           nl+
           'Options can be actually anywhere on the command-line, mixed between' +nl+
           'the required parameters shown in the above "usage" specification.' +nl+
           'Accepted options are:' +nl+
           HelpOptionHelp +nl+
           VersionOptionHelp +nl+
           '  -p / --camera-pos POS.X POS.Y POS.Z ,' +nl+
           '  -d / --camera-dir DIR.X DIR.Y DIR.Z ,' +nl+
           '  -u / --camera-up  UP.X  UP.Y  UP.Z' +nl+
           '                        Set initial camera position, direction and up' +nl+
           '  --view-angle-x ANGLE  Set horizontal viewing angle (in degrees)' +nl+
           '  --force-view-angle-y ANGLE' +nl+
           '                        Set vertical viewing angle (in degrees)' +nl+
           '  --scene-bg-color RED GREEN BLUE' +nl+
           '                        Set color emitted by scene background' +nl+
           '  --write-partial-rows ROWS LOG-ROWS-FILE' +nl+
           '                        Causes partial result to be saved to the' +nl+
           '                        OUT-IMAGE-FILENAME after generating each ROWS rows.' +nl+
           '                        Additionally, in LOG-ROWS-FILE file number' +nl+
           '                        of written rows will be recorded.' +nl+
           '                        Value 0 for ROWS (default) prevents from writing' +nl+
           '                        partial result' +nl+
           '  --first-row ROWS      Assume ROWS rows were already generated and saved' +nl+
           '                        in OUT-IMAGE-FILENAME' +nl+
           VRMLNodesDetailOptionsHelp +nl+
           '  --octree-max-depth DEPTH ,' +nl+
           '  --octree-leaf-capacity CAPACITY' +nl+
           '                        Set the parameters of generated triangle octree' +nl+
           nl+
           'Options meaningfull only for path tracer (ignored if supplied for classic' +nl+
           'raytracer):' +nl+
           '  --direct-illum-samples-count COUNT' +nl+
           '                        Set direct illumination samples count (default 1)' +nl+
           '  --r-roul-continue ALPHA' +nl+
           '                        Set Roussian Roulette parameter for continuation' +nl+
           '                        (e.g. 0.75 gives longer paths than 0.5)' +nl+
           '  --primary-samples-count COUNT' +nl+
           '                        Set primary samples count (default 1)' +nl+
           nl+
           SVrmlEngineProgramHelpSuffix('rayhunter', Version, true));
         ProgramBreak;
        end;
    14: begin
         Writeln(Version);
         ProgramBreak;
        end;
    else raise EInternalError.Create('OptionProc');
   end;
  end;

var
  { rest of helper variables }
  RenderingTime, ViewAngleY: Single;
  PathsCount, PrimaryRaysCount: Cardinal;
  OutImageClass: TImageClass;
  RayTracer: TRayTracer;
  DummyGravityUp: TVector3Single;
begin
 { parsing parameters with no assigned positions }
 VRMLNodesDetailOptionsParse;
 ParseParameters(Options, @OptionProc, nil);
 { parsing parameters with assigned positions }
 Parameters.CheckHighAtLeast(6);
 case ArrayPosText(Parameters[1], ['classic', 'path']) of
  0: begin RTKind := rtkClassic;    Parameters.CheckHigh(6) end;
  1: begin RTKind := rtkPathTracer; Parameters.CheckHigh(7) end;
  else raise EInvalidParams.Create('Invalid RayTracer kind : '+
    'expected "classic" or "path", got ' + Parameters[1]);
 end;
 Parameters.Delete(0, 1);
 RTDepth := StrToInt(Parameters[1]); Parameters.Delete(0, 1);
 if RTKind = rtkPathTracer then
 begin
  PTNonPrimarySamplesCount := StrToInt(Parameters[1]); Parameters.Delete(0, 1);
 end;
 ImageWidth := StrToInt(Parameters[1]); Parameters.Delete(0, 1);
 ImageHeight := StrToInt(Parameters[1]); Parameters.Delete(0, 1);
 SceneFilename := Parameters[1]; Parameters.Delete(0, 1);
 OutImageFilename := Parameters[1]; Parameters.Delete(0, 1);

 { register progres showing on console }
 Progress.UserInterface := ProgressConsoleInterface;

 { init some vars to nil values (to allow simple try..finally..end clause
   instead of nested try..try.. ... finally .. finally ...end) }
 scene := nil;
 Image := nil;

 try
  { read scene and build SceneOctree }
  VRMLWarning := @VRMLWarning_Write;
  Write('Reading scene from file "'+ExtractFileName(sceneFilename)+'"... ');
  scene := TVRMLScene.Create(nil);
  scene.Load(SceneFilename, true);
  Writeln('done.');
  Writeln(scene.Info(true, false, false));

  { calculate Scene.TriangleOctree }
  Scene.TriangleOctreeLimits^.MaxDepth := OctreeMaxDepth;
  Scene.TriangleOctreeLimits^.LeafCapacity := OctreeLeafCapacity;
  Scene.TriangleOctreeProgressTitle := 'Building octree';
  Scene.Spatial := [ssVisibleTriangles];

  { calculate CamPos/Dir/Up }
  scene.GetPerspectiveViewpoint(CamPos, CamDir, CamUp, DummyGravityUp);
  if WasParam_CamPos then CamPos := Param_CamPos;
  if WasParam_CamDir then CamDir := Param_CamDir;
  if WasParam_CamUp  then CamUp := Param_CamUp ;

  { calculate Image }
  { calculate OutImageKind }
  OutImageClass := ImageClassBestForSavingToFormat(OutImageFilename);
  { try load image from OutImageFilename if FirstRow > 0 }
  if FirstRow > 0 then
  try
   Image := LoadImage(OutImageFilename, [OutImageClass], [],
     ImageWidth, ImageHeight);
  except {silence any exception} end;
  { if not FirstRow = 0 or loading from OutImageFilename failed : init clear image }
  if Image = nil then
  begin
   Image := OutImageClass.Create(ImageWidth, ImageHeight);
   { init image. clear image to SceneBGColor - raytracer zapisze caly obrazek
     i zakryje to tlo ale ciagle gdy bedziemy zapisywac write-partial-rows to
     to tlo bedzie widoczne. A nie chcemy zeby byly tam widoczne jakies smieci
     typowe dla niezainicjowanej pamieci. }
   if Image is TRGBEImage then
    Image.Clear(Vector3ToRGBE(SceneBGColor)) else
   if Image is TRGBImage then
    Image.Clear(Vector4Byte(Vector4Single(SceneBGColor, 0))) else
    raise EInternalError.Create('inv OutImageClass');
  end;

  { init ViewAngleY }
  if ForceViewAngleY <> 0.0 then
   ViewAngleY := ForceViewAngleY else
   ViewAngleY := AdjustViewAngleDegToAspectRatio(ViewAngleX, ImageHeight/ImageWidth);

  { create RayTracer instance, set it's properties }
  case RTKind of
    rtkClassic:
      begin
        RayTracer := TClassicRayTracer.Create;
        TClassicRayTracer(RayTracer).InitialDepth := RTDepth;
        TClassicRayTracer(RayTracer).FogNode := Scene.FogNode;
        TClassicRayTracer(RayTracer).FogDistanceScaling := Scene.FogDistanceScaling;
        TClassicRayTracer(RayTracer).HeadLightExists := Scene.HeadLight <> nil;
        if Scene.HeadLight <> nil then
          TClassicRayTracer(RayTracer).HeadLight := Scene.HeadLight.ActiveLight(CamPos, CamDir);
      end;
    rtkPathTracer:
      begin
        RayTracer := TPathTracer.Create;
        TPathTracer(RayTracer).MinDepth := RTDepth;
        TPathTracer(RayTracer).RRoulContinue := PTRRoulContinue;
        TPathTracer(RayTracer).PrimarySamplesCount := PTPrimarySamplesCount;
        TPathTracer(RayTracer).NonPrimarySamplesCount := PTNonPrimarySamplesCount;
        TPathTracer(RayTracer).DirectIllumSamplesCount := PTDirectIllumSamplesCount;
      end;
  end;
  RayTracer.Image := Image;
  RayTracer.Octree := Scene.OctreeVisibleTriangles;
  RayTracer.CamPosition := CamPos;
  RayTracer.CamDirection := CamDir;
  RayTracer.CamUp := CamUp;
  RayTracer.ViewAngleDegX := ViewAngleX;
  RayTracer.ViewAngleDegY := ViewAngleY;
  RayTracer.SceneBGColor := SceneBGColor;
  RayTracer.PixelsMadeNotifier := @PixelsMadeNotify;
  RayTracer.FirstPixel :=  FirstRow * Image.Width;

  { go ! }
  Scene.OctreeVisibleTriangles.DirectCollisionTestsCounter := 0;
  ProcessTimerBegin;
  Progress.Init(ImageHeight-FirstRow, 'Rendering');
  try
   RayTracer.Execute;
  finally Progress.Fini; end;

  { write statistics }
  RenderingTime := ProcessTimerEnd;

  FreeAndNil(RayTracer);

  Writeln(Format(
    'Rendering done in %f seconds.' +nl+
    '%d simple collision tests done (one ray tested with one triangle).',
    [ RenderingTime,
      Scene.OctreeVisibleTriangles.DirectCollisionTestsCounter ]));
  case RTKind of
   rtkClassic: begin
      PrimaryRaysCount := Image.Width * (Image.Height-FirstRow);
      Writeln(Format(
        'Image size is %d x %d pixels (--first-row %d) which gives %d primary rays.' +nl+
        '%f primary rays done per second.' +nl+
        '%f simple collision tests done per one primary ray.',
	[ Image.Width, Image.Height, FirstRow, PrimaryRaysCount,
	  PrimaryRaysCount / RenderingTime,
	  Scene.OctreeVisibleTriangles.DirectCollisionTestsCounter /  PrimaryRaysCount ]));
     end;
   rtkPathTracer: begin
      PathsCount := Image.Width * (Image.Height-FirstRow) * PTPrimarySamplesCount * PTNonPrimarySamplesCount;
      Writeln(Format(
        'Image size is %d x %d pixels (--first-row %d) and we use %d (primary) x %d (non-primary) '+
	'samples per pixel which gives %d paths.' +nl+
        '%f paths done per second.' +nl+
        '%f simple collision tests done per one path.',
	[ Image.Width, Image.Height, FirstRow,
	  PTPrimarySamplesCount, PTNonPrimarySamplesCount, PathsCount,
	  PathsCount / RenderingTime,
	  Scene.OctreeVisibleTriangles.DirectCollisionTestsCounter /  PathsCount ]));
     end;
  end;

  SaveImage(Image, OutImageFilename);
 finally
  Image.Free;
  scene.Free;
 end;
end.

{
  Local Variables:
  kam-compile-release-command-unix:    "./compile.sh && mv -fv rayhunter      ~/bin/"
  kam-compile-release-command-windows: "./compile.sh && mv -fv rayhunter.exe c:\\\\bin\\\\"
  End:
}