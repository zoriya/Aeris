import 'package:flutter_localizations/flutter_localizations.dart';
import 'package:mobile/src/views/startup_page.dart';
import 'package:mobile/src/views/login_page.dart';
import 'package:mobile/src/views/home_page.dart';
import 'package:mobile/src/views/pipeline_detail_page.dart';
import 'package:mobile/src/constants.dart';
import 'package:flutter/material.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

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
        supportedLocales: const [Locale('en', ''), Locale('fr', '')],
        theme: ThemeData(colorScheme: aerisScheme),
        initialRoute: '/home',
        routes: {
          '/': (BuildContext context) => const StartupPage(),
          '/login': (BuildContext context) => const LoginPage(),
          '/home': (BuildContext context) => const HomePage(),
          '/pipeline': (BuildContext context) => const PipelineDetailPage(),
        });
  }
}
