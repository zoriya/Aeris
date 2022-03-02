import 'package:aeris/src/aeris_api.dart';
import 'package:aeris/src/main.dart';
import 'package:aeris/src/widgets/aeris_page.dart';
import 'package:aeris/src/widgets/setup_api_route.dart';
import 'package:flutter_login/flutter_login.dart';
import 'package:flutter/material.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:get_it/get_it.dart';

/// Login Page Widget
class LoginPage extends StatelessWidget {
  const LoginPage({Key? key}) : super(key: key);

  /// Time between login validation and login finish
  Duration get loginDuration => const Duration(milliseconds: 2500);

  /// Called when user clicks on [FlutterLogin] widget 'login' button
  Future<String?> _authUser(LoginData data) async {
    bool connected =
        await GetIt.I<AerisAPI>().createConnection(data.name, data.password);
    if (!connected) {
      return AppLocalizations.of(Aeris.materialKey.currentContext!)
          .usernameOrPasswordIncorrect;
    }
    return null;
  }

  /// Opens signup page of [FlutterLogin] widget
  Future<String?> _signupUser(SignupData data) async {
    bool connected =
        await GetIt.I<AerisAPI>().signUpUser(data.name!, data.password!);
    if (connected == false) {
      return AppLocalizations.of(Aeris.materialKey.currentContext!).errorOnSignup;
    }
    return null;
  }

  @override
  Widget build(BuildContext context) {
    return AerisPage(
        displayAppbar: false,
        body: FlutterLogin(
            disableCustomPageTransformer: true,
            logo: const AssetImage("assets/logo.png"),
            hideForgotPasswordButton: true,
            onRecoverPassword: (_) => null,
            theme: LoginTheme(
                pageColorLight: Colors.transparent,
                pageColorDark: Colors.transparent,
                primaryColor: Theme.of(context).colorScheme.primary),
            onLogin: _authUser,
            onSignup: _signupUser,
            userType: LoginUserType.name,
            userValidator: (input) {
              if (input == null || input.trim().length < 4) return "Must be at least 4 chars long";
              return null;
            },
            onSubmitAnimationCompleted: () {
              Navigator.of(context).pushNamedAndRemoveUntil('/home', (route) => false);
            }));
  }
}
