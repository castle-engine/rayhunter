{
  Copyright 2003-2012 Michalis Kamburelis.

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

uses SysUtils, VectorMath, RayTracer, CastleSceneCore, TriangleOctree,
  CastleImages, CastleUtils, ProgressUnit, ProgressConsole,
  CastleParameters, X3DNodesDetailOptions,
  X3DFields, X3DNodes, RaysWindow, CastleStringUtils, CastleWarnings,
  CastleTimeUtils,
  { TODO: These are OpenGL-specific units, and we would prefer not to use
    them in rayhunter. Scene should be TCastleSceneCore (not TCastleScene),
    and scene manager should be... well, something not related to OpenGL.
    All this trouble is needed now to get BaseLights (containing headlight)
    from scene manager. }
  CastleSceneManager, CastleScene;

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

  PerspectiveView: boolean = true;
  { Was PerspectiveView value explicitly states by command-line param? }
  PerspectiveViewExplicit: boolean = false;
  { PerspectiveViewAngles[1] = 0 means "unspecified",
    will be adjusted to image dims }
  PerspectiveViewAngles: TVector2Single = (60, 0);
  OrthoViewDimensions: TVector4Single = (-1, -1, 1, 1);

  OctreeMaxDepth: integer = DefTriangleOctreeMaxDepth;
  OctreeLeafCapacity: integer = DefTriangleOctreeLeafCapacity;

  WritePartialRows: Cardinal = 0;
  WritePartialRows_LogFile: string = '';

  PTPrimarySamplesCount: Cardinal = 1;
  PTDirectIllumSamplesCount: Cardinal = 1;
  PTRRoulContinue: Single = 0.5;
  FirstRow: Cardinal = 0;

  { helper variables for doing the job --------------------------------------- }
  Scene: TCastleScene;
  Image: TCastleImage;

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
  Version = '1.3.3';
  Options: array[0..15]of TOption =
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
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0; Long: 'ortho'; Argument: oaRequired4Separate)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
   case OptionNum of
    0 : SceneBGColor := SeparateArgsToVector3Single(SeparateArgs);
    1 : begin Param_CamPos := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamPos := true end;
    2 : begin Param_CamDir := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamDir := true end;
    3 : begin Param_CamUp := SeparateArgsToVector3Single(SeparateArgs); WasParam_CamUp := true end;
    4 : begin PerspectiveViewExplicit := true; PerspectiveView := true; PerspectiveViewAngles[0] := StrToFloat(Argument); end;
    5 : begin PerspectiveViewExplicit := true; PerspectiveView := true; PerspectiveViewAngles[1] := StrToFloat(Argument); end;
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
           'RayHunter: ray-tracer for VRML / X3D (and others, like  3DS) models.' +nl+
           'Classic (Whitted-style) ray-tracer (based on local lighting equations' +nl+
           'from VRML 97 / X3D specification) and simple Monte Carlo path tracer are' +nl+
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
           '  --view-angle-x ANGLE  Set horizontal viewing angle (in degrees).' +nl+
           '  --force-view-angle-y ANGLE' +nl+
           '                        Set vertical viewing angle (in degrees).' +nl+
           '  --ortho LEFT BOTTOM RIGHT TOP' +nl+
           '                        Use orthographic projection with given dimensions.' +nl+
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
           X3DNodesDetailOptionsHelp +nl+
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
           SCastleEngineProgramHelpSuffix('rayhunter', Version, true));
         ProgramBreak;
        end;
    14: begin
         Writeln(Version);
         ProgramBreak;
        end;
    15 :begin
          PerspectiveView := false;
          PerspectiveViewExplicit := true;
          OrthoViewDimensions[0] := StrToFloat(SeparateArgs[1]);
          OrthoViewDimensions[1] := StrToFloat(SeparateArgs[2]);
          OrthoViewDimensions[2] := StrToFloat(SeparateArgs[3]);
          OrthoViewDimensions[3] := StrToFloat(SeparateArgs[4]);
        end;
    else raise EInternalError.Create('OptionProc');
   end;
  end;

var
  { rest of helper variables }
  RenderingTime: Single;
  PathsCount, PrimaryRaysCount: Cardinal;
  OutImageClass: TCastleImageClass;
  MyRayTracer: TRayTracer;
  DummyGravityUp: TVector3Single;
  ModelProjectionType: TProjectionType;
  Viewpoint: TAbstractViewpointNode;
  FieldOfView: TMFFloat;
  SceneManager: TCastleSceneManager;
begin
 { parsing parameters with no assigned positions }
 X3DNodesDetailOptionsParse;
 Parameters.Parse(Options, @OptionProc, nil);
 { parsing parameters with assigned positions }
 Parameters.CheckHighAtLeast(6);
 case ArrayPosText(Parameters[1], ['classic', 'path']) of
  0: begin RTKind := rtkClassic;    Parameters.CheckHigh(6) end;
  1: begin RTKind := rtkPathTracer; Parameters.CheckHigh(7) end;
  else raise EInvalidParams.Create('Invalid RayTracer kind : '+
    'expected "classic" or "path", got ' + Parameters[1]);
 end;
 Parameters.Delete(0);
 RTDepth := StrToInt(Parameters[1]); Parameters.Delete(0);
 if RTKind = rtkPathTracer then
 begin
  PTNonPrimarySamplesCount := StrToInt(Parameters[1]); Parameters.Delete(0);
 end;
 ImageWidth := StrToInt(Parameters[1]); Parameters.Delete(0);
 ImageHeight := StrToInt(Parameters[1]); Parameters.Delete(0);
 SceneFilename := Parameters[1]; Parameters.Delete(0);
 OutImageFilename := Parameters[1]; Parameters.Delete(0);

 { register progres showing on console }
 Progress.UserInterface := ProgressConsoleInterface;

 { init some vars to nil values (to allow simple try..finally..end clause
   instead of nested try..try.. ... finally .. finally ...end) }
 Scene := nil;
 Image := nil;
 SceneManager := nil;

 try
  { read scene and build SceneOctree }
  OnWarning := @OnWarningWrite;
  Write('Reading scene from file "'+ExtractFileName(sceneFilename)+'"... ');
  Scene := TCastleScene.Create(nil);
  Scene.Load(SceneFilename, true);
  Writeln('done.');
  Writeln(Scene.Info(true, false, false));

  { calculate Scene.TriangleOctree }
  Scene.TriangleOctreeLimits^.MaxDepth := OctreeMaxDepth;
  Scene.TriangleOctreeLimits^.LeafCapacity := OctreeLeafCapacity;
  Scene.TriangleOctreeProgressTitle := 'Building octree';
  Scene.Spatial := [ssVisibleTriangles];

  { calculate SceneManager (will be used for headlight in BaseLights) }
  SceneManager := TCastleSceneManager.Create(nil);
  SceneManager.MainScene := Scene;
  SceneManager.Items.Add(Scene);

  { calculate CamPos/Dir/Up }
  Viewpoint := Scene.GetViewpoint(ModelProjectionType, CamPos, CamDir, CamUp, DummyGravityUp);

  if not PerspectiveViewExplicit then
  begin
    { If user didn't choose explicitly perspective or orthographic projection,
      then viewpoint node determines it. }
    PerspectiveView := ModelProjectionType = ptPerspective;
    if (Viewpoint <> nil) and
       (Viewpoint is TOrthoViewpointNode) then
    begin
      { So we know that user didn't also explicitly specify ortho dimensions.
        So use the ones from viewpoint. }
      FieldOfView := TOrthoViewpointNode(Viewpoint).FdFieldOfView;
      if FieldOfView.Count > 0 then OrthoViewDimensions[0] := FieldOfView.Items[0];
      if FieldOfView.Count > 1 then OrthoViewDimensions[1] := FieldOfView.Items[1];
      if FieldOfView.Count > 2 then OrthoViewDimensions[2] := FieldOfView.Items[2];
      if FieldOfView.Count > 3 then OrthoViewDimensions[3] := FieldOfView.Items[3];
    end else
    if (Viewpoint <> nil) and
       (Viewpoint is TOrthographicCameraNode_1) then
    begin
      OrthoViewDimensions[0] := -TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
      OrthoViewDimensions[1] := -TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
      OrthoViewDimensions[2] :=  TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
      OrthoViewDimensions[3] :=  TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
    end;
  end;

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
   if Image is TRGBFloatImage then
    TRGBFloatImage(Image).Clear(SceneBGColor) else
   if Image is TRGBImage then
    Image.Clear(Vector4Byte(Vector4Single(SceneBGColor, 0))) else
    raise EInternalError.Create('inv OutImageClass');
  end;

  { init ViewAngleY }
  if PerspectiveViewAngles[1] = 0.0 then
    PerspectiveViewAngles[1] := AdjustViewAngleDegToAspectRatio(
      PerspectiveViewAngles[0], ImageHeight/ImageWidth);

  { create MyRayTracer instance, set it's properties }
  case RTKind of
    rtkClassic:
      begin
        MyRayTracer := TClassicRayTracer.Create;
        TClassicRayTracer(MyRayTracer).InitialDepth := RTDepth;
        TClassicRayTracer(MyRayTracer).FogNode := Scene.FogNode;
        TClassicRayTracer(MyRayTracer).BaseLights := SceneManager.BaseLights;
      end;
    rtkPathTracer:
      begin
        MyRayTracer := TPathTracer.Create;
        TPathTracer(MyRayTracer).MinDepth := RTDepth;
        TPathTracer(MyRayTracer).RRoulContinue := PTRRoulContinue;
        TPathTracer(MyRayTracer).PrimarySamplesCount := PTPrimarySamplesCount;
        TPathTracer(MyRayTracer).NonPrimarySamplesCount := PTNonPrimarySamplesCount;
        TPathTracer(MyRayTracer).DirectIllumSamplesCount := PTDirectIllumSamplesCount;
      end;
  end;
  MyRayTracer.Image := Image;
  MyRayTracer.Octree := Scene.OctreeVisibleTriangles;
  MyRayTracer.CamPosition := CamPos;
  MyRayTracer.CamDirection := CamDir;
  MyRayTracer.CamUp := CamUp;
  MyRayTracer.PerspectiveView := PerspectiveView;
  MyRayTracer.PerspectiveViewAngles := PerspectiveViewAngles;
  MyRayTracer.OrthoViewDimensions := OrthoViewDimensions;
  MyRayTracer.SceneBGColor := SceneBGColor;
  MyRayTracer.PixelsMadeNotifier := @PixelsMadeNotify;
  MyRayTracer.FirstPixel :=  FirstRow * Image.Width;

  { go ! }
  Scene.OctreeVisibleTriangles.DirectCollisionTestsCounter := 0;
  ProcessTimerBegin;
  Progress.Init(ImageHeight-FirstRow, 'Rendering');
  try
   MyRayTracer.Execute;
  finally Progress.Fini; end;

  { write statistics }
  RenderingTime := ProcessTimerEnd;

  FreeAndNil(MyRayTracer);

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
  FreeAndNil(Image);
  FreeAndNil(Scene);
  FreeAndNil(SceneManager);
 end;
end.
