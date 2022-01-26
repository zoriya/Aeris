FROM mingc/android-build-box:latest
COPY pubspec.* ./
RUN flutter pub get

COPY . .
RUN flutter build apk lib/src/main.dart --no-pub
RUN find .
CMD mv ./build/app/outputs/flutter-apk/app-release.apk /dist/aeris_android.apk
