import 'package:mobile/src/views/startup_page.dart';
import 'package:flutter/material.dart';
import 'package:flutter_localizations/flutter_localizations.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  final ColorScheme aerisScheme = const ColorScheme(
    primary: Color.fromRGBO(55, 71, 79, 1),
    secondary: Color.fromRGBO(240, 98, 146, 1),
    background: Colors.white,
    brightness: Brightness.light,
    onError: Colors.red,
    error: Colors.red,
    onPrimary: Colors.white,
    onSecondary: Colors.black,
    onBackground: Colors.black,
    surface: Color(0xFF808080),
    onSurface: Color(0xFF241E30),
  );

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      debugShowCheckedModeBanner: false,
      title: 'Aeris',
      localizationsDelegates: const [
        GlobalMaterialLocalizations.delegate,
        GlobalWidgetsLocalizations.delegate,
        GlobalCupertinoLocalizations.delegate,
      ],
      supportedLocales: const [
        Locale('en', ''),
        Locale('fr', '')
      ],
      theme: ThemeData(
        colorScheme: aerisScheme
      ),
      initialRoute: '/',
      routes: {
        '/': (BuildContext context) => const StartupPage()
      }
    );
  }
}
