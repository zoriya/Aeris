import 'package:flutter/material.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/providers/user_services_provider.dart';
import 'package:mobile/src/widgets/action_card.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:mobile/src/widgets/warning_dialog.dart';
import 'package:provider/provider.dart';

///Page listing connected & available services
class ServicePage extends StatelessWidget {
  const ServicePage({Key? key}) : super(key: key);

  List<Widget> getServiceGroup(String groupName, Icon trailingIcon, void Function() onTap, BuildContext context) {
    UserServiceProvider uServicesProvider = Provider.of<UserServiceProvider>(context);

    return [
      Text("$groupName:",
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
    UserServiceProvider uServiceProvider = Provider.of<UserServiceProvider>(context, listen: false);
    for (var service in services) {
      uServiceProvider.createUserService(service);
    }

    return AerisCardPage(
      body: NotificationListener<OverscrollIndicatorNotification>(
        onNotification: (overscroll) {
          overscroll.disallowIndicator();
          return true;
        },
        child: ListView(
          children: [
            ...[
              const Align(
                alignment: Alignment.center,
                child: Text("Services",
                  style: TextStyle(fontSize: 25)
                ),
              ),
              const SizedBox(height: 60)
            ],
            ...getServiceGroup(
              "Connected",
              const Icon(Icons.delete, color: Colors.red),
              () => showDialog(
                context: context,
                builder: (BuildContext context) => WarningDialog(
                  message: "You are about to disconnect to a service. Once disconnected, every related pipeline will be deleted. This action cannot be undone.",
                  onAccept: () => print("Disconnect") /* TODO Delete action*/,
                  warnedAction: "Disconnect")
                ),
              context
            ),
            ...getServiceGroup(
              "Available",
              const Icon(Icons.connect_without_contact, color: Colors.green),
              () => print("Connected") /* TODO Add action*/,
            context),
          ],
        ),
      ),
    );
  }
}
