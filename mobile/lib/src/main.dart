import 'package:flutter_localizations/flutter_localizations.dart';
import 'package:mobile/src/providers/pipelines_provider.dart';
import 'package:mobile/src/providers/user_services_provider.dart';
import 'package:mobile/src/views/create_pipeline_page.dart';
import 'package:mobile/src/views/pipeline_detail_page.dart';
import 'package:mobile/src/views/service_page.dart';
import 'package:mobile/src/views/setup_action_page.dart';
import 'package:mobile/src/views/startup_page.dart';
import 'package:mobile/src/views/login_page.dart';
import 'package:mobile/src/views/home_page.dart';
import 'package:mobile/src/constants.dart';
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';

void main() {
  runApp(
    MultiProvider(
      providers: [
        ChangeNotifierProvider(create: (_) => PipelineProvider()),
        ChangeNotifierProvider(create: (_) => UserServiceProvider())
      ],
      child: const MyApp()
    )
  );
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  ///This widget is the root of your application.
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
          Map pageRoutes = {
            '/': () => const StartupPage(),
            '/login': () => const LoginPage(),
            '/home': () => const HomePage(),
          };
          Map cardRoutes = {
            '/pipeline': () => const PipelineDetailPage(),
            '/services': () => const ServicePage(),
            '/pipeline/action/mod': () => const SetupActionPage(),
            '/pipeline/new': () => const CreatePipelinePage()
          };
          Map routes = {}
            ..addAll(cardRoutes)
            ..addAll(pageRoutes);
          return PageRouteBuilder(
              opaque: false,
              settings: settings,
              pageBuilder: (_, __, ___) => routes[settings.name].call(),
              transitionDuration: const Duration(milliseconds: 500),
              transitionsBuilder: (context, animation, secondaryAnimation,
                      child) =>
                  pageRoutes.containsKey(settings.name)
                      ? ScaleTransition(
                          child: child,
                          scale: CurvedAnimation(
                            parent: animation,
                            curve: Curves.ease,
                          ))
                      : SlideTransition(
                          position: animation.drive(
                              Tween(begin: const Offset(0, 1), end: Offset.zero)
                                  .chain(CurveTween(curve: Curves.ease))),
                          child: child,
                        ));
        });
  }
}
