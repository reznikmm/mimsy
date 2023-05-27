--  SPDX-FileCopyrightText: 2016-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;
with League.JSON.Objects;

procedure Make_Mission
  (Mission_Dir : League.Strings.Universal_String;
   Station     : League.JSON.Objects.JSON_Object;
   Object      : out League.JSON.Objects.JSON_Object);
