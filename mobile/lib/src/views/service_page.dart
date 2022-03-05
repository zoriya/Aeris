import 'package:flutter/material.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/providers/pipelines_provider.dart';
import 'package:aeris/src/providers/services_provider.dart';
import 'package:aeris/src/widgets/service_card.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:aeris/src/widgets/warning_dialog.dart';
import 'package:provider/provider.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:url_launcher/url_launcher.dart';

///Page listing connected & available services
class ServicePage extends StatelessWidget {
  const ServicePage({Key? key}) : super(key: key);

  List<Widget> getServiceGroup(List<Service> services, String groupName, Icon trailingIcon,
      void Function(Service) onTap, BuildContext context) {
    if (services.isEmpty) return [];
    return [
      Text(
        "$groupName:",
        style: const TextStyle(fontWeight: FontWeight.w500),
      ),
      const SizedBox(height: 10),
      for (var service in services)
        ServiceCard(
            leading: service.getLogo(logoSize: 50),
            title: service.name,
            trailing: IconButton(
              splashColor: trailingIcon.color!.withAlpha(100),
              splashRadius: 20,
              icon: trailingIcon,
              onPressed: () => onTap(service),
            )),
      const SizedBox(height: 30),
    ];
  }

  @override
  Widget build(BuildContext context) {
    return Consumer<ServiceProvider>(
      builder: (context, serviceProvider, _) => Consumer<PipelineProvider>(
      builder: (context, pipelineProvider, _) => AerisCardPage(
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
                serviceProvider.connectedServices,
                AppLocalizations.of(context).connected,
                const Icon(Icons.delete, color: Colors.red),
                (Service service) => showDialog(
                    context: context,
                    builder: (BuildContext context) => WarningDialog(
                        message: AppLocalizations.of(context)
                            .disconnectServiceWarningMessage,
                        onAccept: () => serviceProvider
                            .removeService(service)
                            .then((_) => pipelineProvider.fetchPipelines()),
                        warnedAction: AppLocalizations.of(context).disconnect)),
                context),
            ...getServiceGroup(
                serviceProvider.availableServices,
                AppLocalizations.of(context).available,
                const Icon(Icons.connect_without_contact, color: Colors.green),
                (Service service) {
                      launch(Uri.parse(service.authUrl).toString(), forceSafariVC: false);
                    },
                context),
          ],
        ),
      ),
    ));
  }
}
