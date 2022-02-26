import 'package:aeris/src/aeris_api.dart';
import 'package:flutter/material.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/providers/pipelines_provider.dart';
import 'package:aeris/src/providers/user_services_provider.dart';
import 'package:aeris/src/widgets/action_card.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:aeris/src/widgets/warning_dialog.dart';
import 'package:get_it/get_it.dart';
import 'package:provider/provider.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:url_launcher/url_launcher.dart';

///Page listing connected & available services
class ServicePage extends StatelessWidget {
  ServicePage({Key? key}) : super(key: key);

  ///TODO from an api call, determine what services are plugged
  List<Widget> getServiceGroup(String groupName, Icon trailingIcon,
      void Function(Service) onTap, BuildContext context) {
    UserServiceProvider uServicesProvider =
        Provider.of<UserServiceProvider>(context);

    return [
      Text(
        "$groupName:",
        style: const TextStyle(fontWeight: FontWeight.w500),
      ),
      const SizedBox(height: 10),
      for (var service in uServicesProvider.userServices)
        ActionCard(
            leading: service.serviceProvider.getLogo(logoSize: 50),
            title: service.serviceProvider.name,
            trailing: IconButton(
              splashColor: trailingIcon.color!.withAlpha(100),
              splashRadius: 20,
              icon: trailingIcon,
              onPressed: () => onTap(service.serviceProvider),
            )),
      const SizedBox(height: 30),
    ];
  }

  @override
  Widget build(BuildContext context) {
    List<Service> services = [
      Service.discord(),
      Service.gmail(),
      Service.github(),
      Service.youtube(),
      Service.twitter(),
      Service.spotify()
    ];
    UserServiceProvider uServiceProvider =
        Provider.of<UserServiceProvider>(context, listen: false);
    uServiceProvider.clearProvider();
    for (var service in services) {
      uServiceProvider.createUserService(service);
    }

    return Consumer<PipelineProvider>(
      builder: (context, provider, _) => AerisCardPage(
        body: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            ...[
              Align(
                alignment: Alignment.center,
                child: Text(AppLocalizations.of(context).services,
                    style: const TextStyle(fontSize: 25)),
              ),
              const SizedBox(height: 60)
            ],
            ...getServiceGroup(
                AppLocalizations.of(context).connected,
                const Icon(Icons.delete, color: Colors.red),
                (Service service) => showDialog(
                    context: context,
                    builder: (BuildContext context) => WarningDialog(
                        message: AppLocalizations.of(context)
                            .disconnectServiceWarningMessage,
                        onAccept: () => GetIt.I<AerisAPI>()
                            .disconnectService(service)
                            .then((_) => provider.fetchPipelines()),
                        warnedAction: AppLocalizations.of(context).disconnect)),
                context),
            ...getServiceGroup(
                AppLocalizations.of(context).available,
                const Icon(Icons.connect_without_contact, color: Colors.green),
                (Service service) => {
                      print("Connected") /* TODO open page to connect service*/,
                      launch(Uri.parse(service.authUrl).toString())
                    },
                context),
          ],
        ),
      ),
    );
  }
}
