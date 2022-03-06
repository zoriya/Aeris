import 'package:aeris/src/aeris_api.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/providers/services_provider.dart';
import 'package:flutter/material.dart';
import 'package:get_it/get_it.dart';
import 'package:loading_indicator/loading_indicator.dart';
import 'package:provider/provider.dart';

class AuthorizationPage extends StatelessWidget {
  const AuthorizationPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final route = ModalRoute.of(context)!.settings.name!;
    final code = Uri.parse(route).queryParameters['code']!;
    final segments = Uri.parse(route).pathSegments.toList();
    final serviceName = segments.removeLast();
    final service = Service.factory(serviceName);

    if (segments.removeLast() == 'signin') {
      GetIt.I<AerisAPI>().createConnectionFromService(service, code).then((value) => Navigator.pop(context));
    } else {
      Provider.of<ServiceProvider>(context, listen: false).addService(service, code).then((_) {
        Provider.of<ServiceProvider>(context, listen: false).notifyListeners();
        Navigator.pop(context);
      });
    }
    return Container(
        alignment: Alignment.center,
        child: LoadingIndicator(
          indicatorType: Indicator.ballClipRotateMultiple,
          colors: [Theme.of(context).colorScheme.secondary],
        ));
  }
}
