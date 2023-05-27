--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with GNAT.MD5;

with League.Base_Codecs;
with League.Holders;
with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Text_Codecs;

with Mimsy.Sessions;

package body Servlet.Compile is
   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   function Read_File
     (Name    : League.Strings.Universal_String;
      Decoder : League.Text_Codecs.Text_Codec)
        return League.Strings.Universal_String;

   function MD5_Base_64
     (Text : League.Strings.Universal_String)
        return League.Strings.Universal_String;

   procedure Read_JSON
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Result : out League.JSON.Documents.JSON_Document);

   Prefix : constant String := "/tmp/ada_ru/";

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out Compile_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      JSON     : League.JSON.Documents.JSON_Document;
      Object   : League.JSON.Objects.JSON_Object;
      Messages : League.JSON.Arrays.JSON_Array;

      Id      : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"id");

      Solve   : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"solve");

      Store : constant League.Strings.Universal_String :=
        League.Strings.From_UTF_8_String (Prefix & "store/") & Id;

      File_Name : League.Strings.Universal_String := Store;

      Run_File_Name : constant League.Strings.Universal_String :=
        Store & "/run.txt";

      Solved_File_Name : constant League.Strings.Universal_String :=
        Store & "/solved.txt";

   begin
      if Solve.Is_Empty then
         File_Name.Append ("/gcc-error.txt");
      else
         --  In solve mode we use another log file name to avoid interference
         File_Name.Append ("/gcc-check.txt");
      end if;

      if not Ada.Directories.Exists (Store.To_UTF_8_String) then
         Response.Set_Status (Servlet.HTTP_Responses.Not_Found);

      elsif Ada.Directories.Exists (File_Name.To_UTF_8_String) then
         declare
            Text  : constant League.Strings.Universal_String := Read_File
              (File_Name, League.Text_Codecs.Codec_For_Application_Locale);

            List  : constant League.String_Vectors.Universal_String_Vector :=
              Text.Split (Ada.Characters.Wide_Wide_Latin_1.LF);
         begin
            for J in 1 .. List.Length loop
               declare
                  Parts : constant
                    League.String_Vectors.Universal_String_Vector :=
                      List (J).Split (':');
               begin
                  if Parts.Length >= 4 then
                     declare
                        Message : League.JSON.Objects.JSON_Object;
                     begin
                        Message.Insert
                          (+"file",
                           League.JSON.Values.To_JSON_Value (Parts (1)));

                        Message.Insert
                          (+"line",
                           League.JSON.Values.To_JSON_Value
                             (League.Holders.Universal_Integer'Wide_Wide_Value
                                  (Parts (2).To_Wide_Wide_String)));

                        Message.Insert
                          (+"column",
                           League.JSON.Values.To_JSON_Value
                             (League.Holders.Universal_Integer'Wide_Wide_Value
                                  (Parts (3).To_Wide_Wide_String)));

                        Message.Insert
                          (+"text",
                           League.JSON.Values.To_JSON_Value
                             (Parts.Slice (4, Parts.Length).Join (':')));

                        Messages.Append (Message.To_JSON_Value);
                     end;
                  end if;
               end;
            end loop;

            Object.Insert (+"messages", Messages.To_JSON_Value);
            Object.Insert (+"text", League.JSON.Values.To_JSON_Value (Text));
            Object.Insert
              (+"completed", League.JSON.Values.To_JSON_Value (True));

            if Ada.Directories.Exists (Solved_File_Name.To_UTF_8_String) then
               Object.Insert
                 (+"solved",
                  League.JSON.Values.To_JSON_Value (True));
            end if;

            if Ada.Directories.Exists (Run_File_Name.To_UTF_8_String) then
               Object.Insert
                 (+"output",
                  League.JSON.Values.To_JSON_Value
                    (Read_File
                      (Run_File_Name,
                       League.Text_Codecs.Codec_For_Application_Locale)));
            else
               Object.Insert
                 (+"output",
                  League.JSON.Values.To_JSON_Value (Text));
            end if;

            JSON.Set_Object (Object);
            Response.Set_Status (Servlet.HTTP_Responses.OK);
            Response.Set_Content_Type (+"application/json");
            Response.Set_Character_Encoding (+"utf-8");
            Response.Get_Output_Stream.Write (JSON.To_JSON);
         end;
      else
         Response.Set_Status (Servlet.HTTP_Responses.Service_Unavailable);
         Response.Set_Header (+"Retry-After", +"2");
      end if;

      Response.Set_Header (+"Cache-Control", +"no-cache");
   end Do_Get;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out Compile_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      Session : constant Mimsy.Sessions.HTTP_Session_Access :=
        Mimsy.Sessions.HTTP_Session_Access (Request.Get_Session);

      Info : constant Mimsy.Sessions.User_Info := Session.Get_User_Info;

      Action       : League.Strings.Universal_String := +"compile";
      Text         : League.String_Vectors.Universal_String_Vector;
      Content_Type : constant League.String_Vectors.Universal_String_Vector :=
        Request.Get_Headers (+"Content-Type");
   begin
      if Content_Type.Length = 0 or else
        Content_Type.Element (1) /= +"application/json"
      then
         Response.Set_Status (Servlet.HTTP_Responses.Bad_Request);
         return;
      end if;

      declare
         Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
           Request.Get_Input_Stream;
         JSON   : League.JSON.Documents.JSON_Document;
         Object : League.JSON.Objects.JSON_Object;
         List   : League.JSON.Arrays.JSON_Array;
      begin
         Read_JSON (Stream.all, JSON);

         if JSON.Is_Object then
            Object := JSON.To_JSON_Object;
            List := Object.Value (+"text").To_Array;

            for J in 1 .. List.Length loop
               Text.Append (List.Element (J).To_String);
            end loop;

            if Object.Contains (+"action") then
               Action := Object.Value (+"action").To_String;
            end if;

            if Object.Contains (+"mission") then
               Action.Append (" ");
               Action.Append (Object.Value (+"mission").To_String);
               Action.Append (" ");
               Action.Append (Info.User);
            end if;
         else
            Response.Set_Status (Servlet.HTTP_Responses.Bad_Request);
            return;
         end if;
      end;

      declare
         File : Ada.Wide_Wide_Text_IO.File_Type;

         Value : constant League.Strings.Universal_String :=
           Text.Join (Ada.Characters.Wide_Wide_Latin_1.LF);

         Hash : constant League.Strings.Universal_String :=
           MD5_Base_64 (Value);

         Store : constant String :=
           Prefix & League.Strings.To_UTF_8_String ("store/" & Hash);
      begin
         if not Ada.Directories.Exists (Store) then
            Ada.Directories.Create_Directory (Store);
         end if;

         Ada.Wide_Wide_Text_IO.Create
           (File, Name => Store & "/_source.adb", Form => "WCEM=8");

         Ada.Wide_Wide_Text_IO.Put (File, Value.To_Wide_Wide_String);
         Ada.Wide_Wide_Text_IO.Close (File);

         Ada.Wide_Wide_Text_IO.Create
           (File,
            Name => Prefix & "/jobs/" & Hash.To_UTF_8_String,
            Form => "WCEM=8");

         Ada.Wide_Wide_Text_IO.Put_Line (File, Action.To_Wide_Wide_String);
         Ada.Wide_Wide_Text_IO.Close (File);

         declare
            JSON   : League.JSON.Documents.JSON_Document;
            Object : League.JSON.Objects.JSON_Object;
         begin
            Object.Insert
              (+"id",
               League.JSON.Values.To_JSON_Value (Hash));

            JSON.Set_Object (Object);
            Response.Set_Status (Servlet.HTTP_Responses.OK);
            Response.Set_Content_Type (+"application/json");
            Response.Set_Character_Encoding (+"utf-8");
            Response.Get_Output_Stream.Write (JSON.To_JSON);
         end;
      end;
   end Do_Post;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Compile_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"Servlet to manage Ada Compiles";
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Compile_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      if not Ada.Directories.Exists (Prefix) then
         Ada.Directories.Create_Directory (Prefix);
      end if;

      if not Ada.Directories.Exists (Prefix & "store/") then
         Ada.Directories.Create_Directory (Prefix & "store/");
      end if;

      if not Ada.Directories.Exists (Prefix & "jobs/") then
         Ada.Directories.Create_Directory (Prefix & "jobs/");
      end if;

      return Result : Compile_Servlet;
   end Instantiate;

   -------------------
   -- Axe.Read_File --
   -------------------

   function Read_File
     (Name    : League.Strings.Universal_String;
      Decoder : League.Text_Codecs.Text_Codec)
        return League.Strings.Universal_String
   is
      File_Name : constant String :=
        League.Text_Codecs.To_Exception_Message (Name);

      Size      : constant Ada.Directories.File_Size :=
        Ada.Directories.Size (File_Name);

      Length : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Count (Size);

      File   : Ada.Streams.Stream_IO.File_Type;
      Data   : Ada.Streams.Stream_Element_Array (1 .. Length);
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, File_Name, "SHARED=NO");
      Ada.Streams.Stream_IO.Read (File, Data, Last);
      Ada.Streams.Stream_IO.Close (File);

      return Decoder.Decode (Data (1 .. Last));
   end Read_File;

   -----------------
   -- MD5_Base_64 --
   -----------------

   function MD5_Base_64
     (Text : League.Strings.Universal_String)
        return League.Strings.Universal_String
   is
      Digest : constant GNAT.MD5.Binary_Message_Digest :=
        GNAT.MD5.Digest (Text.To_UTF_8_String);
      Vector : constant League.Stream_Element_Vectors.Stream_Element_Vector :=
        League.Stream_Element_Vectors.To_Stream_Element_Vector (Digest);
   begin
      return League.Base_Codecs.To_Base_64_URL (Vector);
   end MD5_Base_64;

   ---------------
   -- Read_JSON --
   ---------------

   procedure Read_JSON
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Result : out League.JSON.Documents.JSON_Document)
   is
      Data   : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last   : Ada.Streams.Stream_Element_Offset := 0;
      Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      loop
         Stream.Read (Data, Last);
         exit when Last in 0;
         Vector.Append (Data (1 .. Last));
      end loop;

      Result := League.JSON.Documents.From_JSON (Vector);
   end Read_JSON;

end Servlet.Compile;
