import 'package:flutter_localizations/flutter_localizations.dart';
import 'package:form_builder_validators/localization/l10n.dart';
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
import 'package:flutter_gen/gen_l10n/app_localizations.dart';

void main() {
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
        locale: const Locale('fr', ''),
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
          Map pageRoutes = {
            '/': () => const StartupPage(),
            '/login': () => const LoginPage(),
            '/home': () => const HomePage(),
          };
          Map cardRoutes = {
            '/pipeline': () => const PipelineDetailPage(),
            '/services': () => const ServicePage(),
            '/pipeline/action/mod': () => const SetupActionPage(),
            '/pipeline/action/new': () => const SetupActionPage(),
            '/pipeline/new': () => const CreatePipelinePage()
          };
          Map routes = {}
            ..addAll(cardRoutes)
            ..addAll(pageRoutes);
          return PageRouteBuilder(
              opaque: false,
              settings: settings,
              pageBuilder: (_, __, ___) => routes[settings.name].call(),
              transitionDuration: const Duration(milliseconds: 350),
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
