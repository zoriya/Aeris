import 'package:flutter/material.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/widgets/action_card.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:mobile/src/widgets/warning_dialog.dart';

// Page listing connected & available services
class ServicePage extends StatelessWidget {
  const ServicePage({Key? key}) : super(key: key);

  List<Widget> getServiceGroup(String groupName, List<Service> services,
      Icon trailingIcon, void Function() onTap, BuildContext context) {
    return [
      Text(
        "$groupName:",
        style: const TextStyle(fontWeight: FontWeight.w500),
      ),
      const SizedBox(height: 10),
      for (var service in services)
        ActionCard(
            leading: service.getLogo(logoSize: 50),
            title: service.name,
            trailing: IconButton(
              splashColor: trailingIcon.color!.withAlpha(100),
              splashRadius: 20,
              icon: trailingIcon,
              onPressed: onTap,
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
    return AerisCardPage(
        body: NotificationListener<OverscrollIndicatorNotification>(
            onNotification: (overscroll) {
              overscroll.disallowIndicator();
              return true;
            },
            child: ListView(
              ///crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                ...[
                  const Align(
                    alignment: Alignment.center,
                    child: Text("Services",
                        style: TextStyle(
                          fontSize: 25,
                        )),
                  ),
                  const SizedBox(height: 60)
                ],
                ...getServiceGroup(
                    "Connected",
                    services,
                    const Icon(Icons.delete, color: Colors.red),
                    () => showDialog(
                        context: context,
                        builder: (BuildContext context) => WarningDialog(
                            message:
                                "You are about to disconnect to a service. Once disconnected, every related pipeline will be deleted. This action cannot be undone.",
                            onAccept: () =>
                                print("Disconnect") /* TODO Delete action*/,
                            warnedAction: "Disconnect")),
                    context),
                ...getServiceGroup(
                    "Available",
                    services,
                    const Icon(Icons.connect_without_contact,
                        color: Colors.green),
                    () => print("Connected") /* TODO Add action*/,
                    context),
              ],
            )));
  }
}
