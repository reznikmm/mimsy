--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

private with Ada.Calendar;
private with Ada.Containers.Hashed_Maps;
private with Ada.Numerics.Discrete_Random;
private with Ada.Streams;

with League.Strings;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;
private with League.Strings.Hash;

package Servlet.OAuth2_Server is

   type OAuth2_Server_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   type OAuth2_Server_Servlet_Access is
     access all OAuth2_Server_Servlet'Class;

private

   package Stream_Element_Random is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Ada.Streams.Stream_Element);

   type Authorization_Code is record
      Code : League.Strings.Universal_String;
      Expire : Ada.Calendar.Time;
   end record;

   package Code_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Authorization_Code,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=");

   type OAuth2_Server_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with
   record
      Random  : Stream_Element_Random.Generator;
      Codes   : Code_Maps.Map;
      Secret  : Ada.Streams.Stream_Element_Array (1 .. 32);
   end record;

   overriding procedure Do_Get
    (Self     : in out OAuth2_Server_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding procedure Do_Post
    (Self     : in out OAuth2_Server_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : OAuth2_Server_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return OAuth2_Server_Servlet;

end Servlet.OAuth2_Server;
