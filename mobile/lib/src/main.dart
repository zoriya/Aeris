import 'package:aeris/src/models/aeris_api.dart';
import 'package:flutter_localizations/flutter_localizations.dart';
import 'package:form_builder_validators/localization/l10n.dart';
import 'package:aeris/src/providers/pipelines_provider.dart';
import 'package:aeris/src/providers/user_services_provider.dart';
import 'package:aeris/src/views/startup_page.dart';
import 'package:aeris/src/views/login_page.dart';
import 'package:aeris/src/views/home_page.dart';
import 'package:aeris/src/constants.dart';
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:get_it/get_it.dart';

void main() {
  AerisAPI interface = AerisAPI();
  GetIt.I.registerSingleton<AerisAPI>(interface);
  runApp(MultiProvider(providers: [
    ChangeNotifierProvider(create: (_) => PipelineProvider()),
    ChangeNotifierProvider(create: (_) => UserServiceProvider())
  ], child: const Aeris()));
}

class Aeris extends StatelessWidget {
  static GlobalKey<NavigatorState> materialKey = GlobalKey<NavigatorState>();
  const Aeris({Key? key}) : super(key: key);

  ///This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
        navigatorKey: Aeris.materialKey,
        debugShowCheckedModeBanner: false,
        title: 'Aeris',
        localizationsDelegates: const [
          AppLocalizations.delegate,
          GlobalMaterialLocalizations.delegate,
          GlobalWidgetsLocalizations.delegate,
          GlobalCupertinoLocalizations.delegate,
          FormBuilderLocalizations.delegate
        ],
        supportedLocales: const [Locale('fr', ''), Locale('en', '')],
        theme: ThemeData(colorScheme: aerisScheme),
        initialRoute: '/home',
        onGenerateRoute: (settings) {
          Map routes = {
            '/': () => const StartupPage(),
            '/login': () => const LoginPage(),
            '/home': () => const HomePage(),
          };
          return PageRouteBuilder(
              opaque: false,
              settings: settings,
              pageBuilder: (_, __, ___) => routes[settings.name].call(),
              transitionDuration: const Duration(milliseconds: 350),
              transitionsBuilder: (context, animation, secondaryAnimation,
                      child) => ScaleTransition(
                          child: child,
                          scale: CurvedAnimation(
                            parent: animation,
                            curve: Curves.ease,
                          ))
                      );
        });
  }
}
