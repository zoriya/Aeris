import 'package:aeris/src/main.dart';
import 'package:aeris/src/widgets/aeris_page.dart';
import 'package:flutter_login/flutter_login.dart';
import 'package:flutter/material.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';

const users = {
  'dribbble@gmail.com': '12345',
  'hunter@gmail.com': 'hunter',
};

/// Login Page Widget
class LoginPage extends StatelessWidget {
  const LoginPage({Key? key}) : super(key: key);

  /// Time between login validation and login finish
  Duration get loginDuration => const Duration(milliseconds: 2500);

  /// Called when user clicks on [FlutterLogin] widget 'login' button
  Future<String?> _authUser(LoginData data) {
    debugPrint('Name: ${data.name}, Password: ${data.password}');
    return Future.delayed(loginDuration).then((_) {
      if (!users.containsKey(data.name)) {
        return AppLocalizations.of(Aeris.materialKey.currentContext!)
            .usernameOrPasswordIncorrect;
      }
      if (users[data.name] != data.password) {
        return AppLocalizations.of(Aeris.materialKey.currentContext!)
            .usernameOrPasswordIncorrect;
      }
      return null;
    });
  }

  /// Opens signup page of [FlutterLogin] widget
  Future<String?> _signupUser(SignupData data) {
    
    return Future.delayed(loginDuration).then((_) {
      return null;
    });
  }

  /// Opens user password recovery page
  Future<String?> _recoverPassword(String name) {
    return Future.delayed(loginDuration).then((_) {
      if (!users.containsKey(name)) {
        return AppLocalizations.of(Aeris.materialKey.currentContext!)
            .userDoesNotExist;
      }
      return null;
    });
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
            onSubmitAnimationCompleted: () {
              Navigator.of(context).popUntil((route) => route.isFirst);
              Navigator.of(context).popAndPushNamed("/home");
            }));
  }
}
