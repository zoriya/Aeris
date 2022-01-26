import 'package:mobile/src/widgets/aeris_page.dart';
import 'package:flutter_login/flutter_login.dart';
import 'package:flutter/material.dart';

const users = {
  'dribbble@gmail.com': '12345',
  'hunter@gmail.com': 'hunter',
};

/// Login Page
class LoginPage extends StatelessWidget {
  const LoginPage({Key? key}) : super(key: key);

  Duration get loginDuration => const Duration(milliseconds: 2500);

  Future<String?> _authUser(LoginData data) {
    debugPrint('Name: ${data.name}, Password: ${data.password}');
    return Future.delayed(loginDuration).then((_) {
      if (!users.containsKey(data.name)) {
        return 'User does not exists';
      }
      if (users[data.name] != data.password) {
        return 'Password does not match';
      }
      return null;
    });
  }

  Future<String?> _signupUser(SignupData data) {
    debugPrint('Signup Name: ${data.name}, Password: ${data.password}');
    return Future.delayed(loginDuration).then((_) {
      return null;
    });
  }

  Future<String?> _recoverPassword(String name) {
    debugPrint('Name: $name');
    return Future.delayed(loginDuration).then((_) {
      if (!users.containsKey(name)) {
        return 'User does not exists';
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
        onRecoverPassword: _recoverPassword,
        theme: LoginTheme(
          pageColorLight: Colors.transparent,
          pageColorDark: Colors.transparent,
          primaryColor: Theme.of(context).colorScheme.primary
        ),
        onLogin: _authUser,
        onSignup: _signupUser,
        onSubmitAnimationCompleted: () {
          Navigator.of(context).popUntil((route) => route.isFirst);
          Navigator.of(context).popAndPushNamed("/home");
        }
      )
    );
  }
}
