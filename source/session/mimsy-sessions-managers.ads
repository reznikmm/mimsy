--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Servlet.OAuth;
with Spikedog.HTTP_Session_Managers;

private with Ada.Containers.Hashed_Maps;
private with Ada.Numerics.Discrete_Random;
private with Ada.Streams;
private with League.Strings.Hash;
private with Servlet.HTTP_Requests;
private with Servlet.HTTP_Responses;

private with Mimsy.Databases;

package Mimsy.Sessions.Managers is

   type HTTP_Session_Manager is limited
   new Spikedog.HTTP_Session_Managers.HTTP_Session_Manager
   and Servlet.OAuth.Login_Handler with private;

   type HTTP_Session_Manager_Access is access all HTTP_Session_Manager'Class;

   procedure Initialize
     (Self   : in out HTTP_Session_Manager);

private

   type Session_Access is access all Servlet.HTTP_Sessions.HTTP_Session'Class;

   package Session_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      Session_Access,
      League.Strings.Hash,
      League.Strings."=");

   package Stream_Element_Random is new Ada.Numerics.Discrete_Random
     (Ada.Streams.Stream_Element);

   type HTTP_Session_Manager is limited
     new Spikedog.HTTP_Session_Managers.HTTP_Session_Manager
       and Servlet.OAuth.Login_Handler
   with record
      Random : Stream_Element_Random.Generator;
      Map    : Session_Maps.Map;
      Pool   : aliased Databases.SQL_Database_Pool;
   end record;

   overriding function Is_Session_Identifier_Valid
    (Self       : HTTP_Session_Manager;
     Identifier : League.Strings.Universal_String) return Boolean;

   overriding function Get_Session
    (Self       : in out HTTP_Session_Manager;
     Identifier : League.Strings.Universal_String)
       return access Servlet.HTTP_Sessions.HTTP_Session'Class;

   overriding function New_Session
    (Self : in out HTTP_Session_Manager)
       return access Servlet.HTTP_Sessions.HTTP_Session'Class;

   overriding procedure Change_Session_Id
    (Self    : in out HTTP_Session_Manager;
     Session : not null access Servlet.HTTP_Sessions.HTTP_Session'Class);

   overriding procedure Do_Login
    (Self     : in out HTTP_Session_Manager;
     Info     : Sessions.User_Info;
     Path     : League.Strings.Universal_String;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

end Mimsy.Sessions.Managers;
