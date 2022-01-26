import 'package:flutter/material.dart';

const Color primaryColor = Color.fromRGBO(55, 71, 79, 1);

const Color secondaryColor = Color.fromRGBO(240, 98, 146, 1);

const ColorScheme aerisScheme = ColorScheme(
  primary: primaryColor,
  secondary: secondaryColor,
  background: primaryColor,
  brightness: Brightness.light,
  onError: Colors.red,
  error: Colors.red,
  onPrimary: Colors.white,
  onSecondary: Colors.white,
  onBackground: Colors.white,
  surface: Color(0xFF808080),
  onSurface: Color(0xFF241E30),
);
