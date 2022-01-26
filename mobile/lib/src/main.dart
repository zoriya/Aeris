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
        onGenerateRoute: (settings) {
          Map routes = {
            '/': () => const StartupPage(),
            '/login': () => const LoginPage(),
            '/home': () => const HomePage(),
            '/pipeline': () => const PipelineDetailPage(),
          };
          if (settings.name == Navigator.defaultRouteName) {
            return null;
          }
          // return PageRouteBuilder(
          //     settings: settings,
          //     opaque: false,
          //     pageBuilder: (_, __, ___) => routes[settings.name].call(),
          //     transitionDuration: const Duration(milliseconds: 500),
          //     transitionsBuilder: (context, animation, secAnimation, child) {
          //       return ScaleTransition(
          //           scale: CurvedAnimation(
          //               parent: animation, curve: Curves.easeOutBack),
          //           child: child,
          //           alignment: Alignment.center);
          //     });
          return PageRouteBuilder(
              opaque: false,
              settings: settings,
              pageBuilder: (_, __, ___) => routes[settings.name].call(),
              transitionDuration: const Duration(milliseconds: 500),
              transitionsBuilder:
                  (context, animation, secondaryAnimation, child) =>
                      SlideTransition(
                        position: animation.drive(
                            Tween(begin: const Offset(0, 1.0), end: Offset.zero)
                                .chain(CurveTween(curve: Curves.ease))),
                        child: child,
                      ));
        });
  }
}
