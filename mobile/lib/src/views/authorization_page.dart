import 'package:aeris/src/aeris_api.dart';
import 'package:aeris/src/models/service.dart';
import 'package:flutter/material.dart';
import 'package:get_it/get_it.dart';
import 'package:loading_indicator/loading_indicator.dart';

class AuthorizationPage extends StatelessWidget {
  const AuthorizationPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final route = ModalRoute.of(context)!.settings.name!;
    final code = Uri.parse(route).queryParameters['code']!;
    final serviceName = Uri.parse(route).pathSegments.last;
    final service = Service.all()
        .firstWhere((element) => element.name.toLowerCase() == serviceName);
    GetIt.I<AerisAPI>()
        .connectService(service, code)
        .then((_) => Navigator.pop(context));
    return Container(
        alignment: Alignment.center,
        child: LoadingIndicator(
          indicatorType: Indicator.ballClipRotateMultiple,
          colors: [Theme.of(context).colorScheme.secondary],
        ));
  }
}
