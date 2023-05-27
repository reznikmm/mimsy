--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Game_Solutions is

   type Solution_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

private

   type Solution_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      Token    : League.Strings.Universal_String;
   end record;

   overriding procedure Do_Get
    (Self     : in out Solution_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Solution_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Solution_Servlet;

end Servlet.Game_Solutions;
