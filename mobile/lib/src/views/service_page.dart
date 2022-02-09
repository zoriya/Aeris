import 'package:flutter/material.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/providers/pipelines_provider.dart';
import 'package:aeris/src/providers/user_services_provider.dart';
import 'package:aeris/src/widgets/action_card.dart';
import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:aeris/src/widgets/warning_dialog.dart';
import 'package:provider/provider.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';

///Page listing connected & available services
class ServicePage extends StatelessWidget {
  const ServicePage({Key? key}) : super(key: key);

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
    List<Service> services = const [
      Service.discord(),
      Service.gmail(),
      Service.github(),
      Service.youtube(),
      Service.twitter(),
      Service.spotify()
    ];
    UserServiceProvider uServiceProvider =
        Provider.of<UserServiceProvider>(context, listen: false);
    for (var service in services) {
      uServiceProvider.createUserService(service);
    }

    return Consumer<PipelineProvider>(
      builder: (context, provider, _) => AerisCardPage(
        body: Column(
            children: [
              ...[
                const Align(
                  alignment: Alignment.center,
                  child: Text("Services", style: TextStyle(fontSize: 25)),
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
                          onAccept: () => {
                                provider.pipelineCollection.pipelines
                                    .removeWhere((Pipeline pipeline) {
                                  if (pipeline.trigger.service == service) {
                                    return true;
                                  }
                                  if (pipeline.reactions
                                      .where((Reaction react) =>
                                          react.service == service)
                                      .isNotEmpty) {
                                    return true;
                                  }
                                  return false;
                                }),

                                /// TODO Remove service from provider
                                provider.notifyListeners(),
                                print("Disconnect")
                              } /* TODO Delete service form db + related actions*/,
                          warnedAction:
                              AppLocalizations.of(context).disconnect)),
                  context),
              ...getServiceGroup(
                  AppLocalizations.of(context).available,
                  const Icon(Icons.connect_without_contact,
                      color: Colors.green),
                  (Service service) =>
                      print("Connected") /* TODO open page to connect service*/,
                  context),
            ],
          ),
        ),
      );
  }
}
