# Local user access

For the user-facing management procedures and the local SharePoint/cache versus
server-hosted scenarios, see the guide chapter
[Manage accounts and deployment](../guide/_content/access-management.qmd).
Accounts belong to an OS user's configuration and pond path: they are not
synchronised across workstations through SharePoint. Flow cache controls currently
require an administrator; ordinary viewer/editor accounts cannot use them.

DataPond requires a verified account before showing a pond's workspace. Selecting
a seeded name no longer signs in. This release supports offline accounts; there
is no email service or automatic email verification.

## First administrator

Install the package dependencies, including `sodium`. Linux source installations
need libsodium development headers (`libsodium-dev` on Debian/Ubuntu).

Open the pond once to initialise its catalogue, then stop the app. In the **local
R console**, create the first administrator and keep the returned code private:

```r
code <- datapond::setup_pond_access(
  data_pond = "/path/to/your/pond",
  username = "your.username",
  first_name = "Your",
  last_name = "Name",
  email = "you@example.org"
)
code
```

For an existing metadata person, supply their numeric `person_id` instead of
`first_name` and `last_name`. Check the ID in the catalogue first. Existing
people, user IDs, administrative flags, and historical attribution are preserved;
legacy flags do not grant access to the authenticated app.

Start DataPond, choose **Activate / recover access**, enter the username and code,
and set a passphrase of at least 15 characters. Then sign in. Codes expire after
24 hours and work only once. Setup refuses to replace an existing account store
and cannot run inside a browser session.

## New colleagues and existing seeded people

1. The colleague chooses **Request access** and supplies their name, username,
   optional contact email, and organisation. The request cannot read or edit data.
2. An administrator signs in and opens **Configuration → Manage accounts**.
3. After verifying the person's identity, select the pending request, role, and
   either **Create a new person** or the correct existing metadata person.
   Matching names or email addresses never automatically link accounts.
4. Choose **Approve pending request** and give the displayed code privately to
   the verified person, for example in person. No email is sent automatically.
5. The colleague activates the account, sets their passphrase, and signs in.

The Parties checkbox maintains metadata attribution users only. It cannot create
login accounts. People linked to login accounts cannot be merged or deactivated
through Parties; use account management for access changes.

## Permissions

| Role | Access |
| --- | --- |
| Viewer | Browse the authenticated pond's metadata and maps. No editing or file/package downloads. |
| Editor | Viewer access plus metadata editing, scans, dataset creation, imports, exports and lifecycle actions, subject to existing ownership restrictions. |
| Pond administrator | Editor access plus account approval, roles, recovery, shared parties/vocabularies, review acceptance and Flow/file movement. |

Downloads are limited to editors and administrators in this release. Licensing
and embargo fields describe dataset policy; they are not a separate per-dataset
permission engine. Grant pond access only to people allowed to see its metadata.
Browser-side table copy/export features can reproduce metadata already visible
to a viewer; viewer access is not a prohibition on copying visible information.

Connected ponds must be opened and authenticated separately. Signing in to one
pond does not unlock another. There is no anonymous workspace browsing.

## Recovery, disabling and audit

After verifying identity, an administrator can select an active or invited account
and choose **Issue activation / recovery code**. A new code supersedes any older
code and revokes existing sessions. Redeeming the code changes the passphrase and
revokes sessions again. The administrator cannot see the passphrase.

An administrator recovering their own account must ask another administrator or
use the trusted local R console:

```r
datapond::recover_pond_access("/path/to/your/pond", "your.username")
```

Use **Apply role and status** to disable or re-enable an activated account or
change its role. Changes revoke its sessions. The final active/invited
administrator cannot be removed through account management. Access events appear
in **Manage accounts → Access audit**, with UTC timestamps and account IDs.

To reopen a rejected request, set it to **Pending approval** and approve it after
verification. To re-enable an invitation that was disabled before activation,
choose **Awaiting activation**, then issue a new activation code.

Sign out explicitly when finished. Sessions expire after 30 minutes without
accepted activity, with an absolute limit of 12 hours. Protected actions recheck
the account and role. Running or queued Flow jobs count as activity while the app
remains connected, except private sign-in/configuration prompts. Sign-out, revoked
access, app closure and the 12-hour limit still cancel jobs. After a job ends,
normal idle expiry resumes. The UI detects revoked/expired sessions within five
seconds.
Login and code-entry throttling persists across browser restarts. Five unsuccessful
attempts for a username require a 15-minute wait; a verified recovery clears the
login throttle. Access requests are also rate limited.

## Storage and deployment boundary

Credentials, hashed session tokens, recovery tokens and the access audit are stored
under the installation's `app_state_dir()/security/`, separately from each pond's
portable `metadata.db`. Stores are keyed by the canonical pond path. Back up this
security directory separately and keep it private. Moving a pond or opening it on
another installation requires local account setup; dataset transfers and remote
pond publication do not transfer login rights or credentials.

Password hashing uses the maintained `sodium` package's salted **scrypt** password
storage and verification functions. This is the available library implementation
for this release; the earlier Argon2id recommendation is not implemented. Tokens
use 32 cryptographically random bytes and only their SHA-256 digests are stored.
Passphrases and codes are never put in the access audit or pond configuration.

This protects access through the application, not against the owner of its R
process, files, or operating-system account. Local R scripts remain a trusted
maintenance interface. An administrator with Flow access can operate on files
accessible to that OS account. Use OS permissions and protected backups for the
underlying data. Keep the default loopback listener for local use; a remotely
hosted deployment requires HTTPS, a controlled service account, and protected
storage. This release does not add SSO or MFA.
